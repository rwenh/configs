-- lua/plugins/specs/dap.lua — Debug Adapter Protocol
--

-- ── Exception-breakpoint picker ───────────────────────────────────────────────
-- File scope + spec `keys` (with rhs) so the mapping exists before nvim-dap loads.
local function exception_breakpoints()
  local ok, dap = pcall(require, "dap")
  if not ok then vim.notify("[dap] nvim-dap not available", vim.log.levels.WARN); return end

  local session = dap.session()
  if not session then
    vim.notify("[dap] No active debug session", vim.log.levels.WARN)
    return
  end

  local filters = (session.capabilities or {}).exceptionBreakpointFilters or {}
  if #filters == 0 then
    vim.notify("[dap] Adapter does not support exception breakpoints", vim.log.levels.INFO)
    return
  end

  local items = vim.tbl_map(function(f)
    return { id = f.filter, label = (f.label or f.filter) .. (f.default and " (default)" or "") }
  end, filters)
  local labels = vim.tbl_map(function(i) return i.label end, items)

  vim.ui.select(labels, { prompt = "Exception breakpoint filter:" }, function(choice, idx)
    if not choice then return end
    local selected = items[idx] and items[idx].id or choice
    pcall(function()
      session:request("setExceptionBreakpoints", { filters = { selected } }, function(err)
        if err then
          vim.notify("[dap] setExceptionBreakpoints error: " .. tostring(err), vim.log.levels.WARN)
        else
          vim.notify("[dap] Exception breakpoint set: " .. selected, vim.log.levels.INFO)
        end
      end)
    end)
  end)
end

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = { "rcarriga/nvim-dap-ui", "theHamsta/nvim-dap-virtual-text", "nvim-neotest/nvim-nio" },
    -- F5–F11 and <leader>;{b,c,...} are owned by core/keymaps.lua, whose handlers
    -- require("dap") (which lazy-loads it). Listing them here as rhs-less lazy keys
    -- installed stubs that shadowed those mappings and never restored them.
    keys = {
      { "<leader>;E", exception_breakpoints, desc = "DAP: Configure exception breakpoints" },
    },
    config = function()
      local dap   = require("dap")
      local dapui = require("dapui")
      local mason = require("core.util.mason")

      -- ── DAP UI ─────────────────────────────────────────────────────────────
      pcall(function()
        dapui.setup({
          icons    = { expanded = "▾", collapsed = "▸", current_frame = "▸" },
          layouts  = {
            { elements = { {id="scopes",size=0.25},{id="breakpoints",size=0.25},{id="stacks",size=0.25},{id="watches",size=0.25} }, size = 40, position = "left" },
            { elements = { {id="repl",size=0.5},{id="console",size=0.5} }, size = 10, position = "bottom" },
          },
          controls = { enabled = true, element = "repl",
            icons = { pause="",play="",step_into="",step_over="",step_out="",step_back="",run_last="▶▶",terminate="⏹" } },
          floating = { max_width = 0.9, max_height = 0.5, border = "rounded" },
        })
      end)

      -- ── Signs ──────────────────────────────────────────────────────────────
      local signs = {
        DapBreakpoint          = { text = "●", texthl = "DapBreakpoint",  linehl = "",              numhl = "" },
        DapStopped             = { text = "▶", texthl = "DapStopped",     linehl = "DapStoppedLine", numhl = "" },
        DapBreakpointCondition = { text = "◆", texthl = "DapBreakpoint",  linehl = "",              numhl = "" },
        DapBreakpointRejected  = { text = "✖", texthl = "DapBreakpoint",  linehl = "",              numhl = "" },
        DapLogPoint            = { text = "◉", texthl = "DapLogPoint",    linehl = "",              numhl = "" },
      }
      for sign, cfg in pairs(signs) do pcall(vim.fn.sign_define, sign, cfg) end

      -- ── Listeners ──────────────────────────────────────────────────────────
      local function safe_open()  pcall(function() dapui.open()  end) end
      local function safe_close() pcall(function() dapui.close() end) end
      dap.listeners.after.event_initialized["dapui_config"]  = safe_open
      dap.listeners.before.event_terminated["dapui_config"]  = safe_close
      dap.listeners.before.event_exited["dapui_config"]      = safe_close

      -- ── register_adapter helper ────────────────────────────────────────────
      -- dap.configurations is keyed by FILETYPE, dap.adapters by adapter name. The old
      -- code stored configs under the adapter name ("delve", "mix_task"), so <F5> in a
      -- Go/Elixir buffer found no configuration at all.
      local function register_adapter(name, filetypes, check_fn, configs, warn_msg)
        local adapter = check_fn()
        if adapter then
          dap.adapters[name] = adapter
          for _, ft in ipairs(filetypes) do dap.configurations[ft] = configs end
        else
          vim.schedule(function() vim.notify(warn_msg, vim.log.levels.WARN) end)
        end
      end

      -- ── Per-project .nvim-dap.lua override ─────────────────────────────────
      local _project_dap_cache = {}

      local _project_dap_loaded = false

      local function load_project_dap_config()
        if _project_dap_loaded then return end

        local ok_path, path_util = pcall(require, "core.util.path")
        local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
        if not root or root == "" then _project_dap_loaded = true; return end
        if _project_dap_cache[root] then _project_dap_loaded = true; return end

        local cfg_file = root .. "/.nvim-dap.lua"
        if vim.fn.filereadable(cfg_file) ~= 1 then
          _project_dap_cache[root] = true
          _project_dap_loaded = true
          return
        end

        local ok, result = require("core.util.secure").load(cfg_file)   -- trust-gated, not dofile
        if not ok or type(result) ~= "table" then
          vim.notify("[dap] .nvim-dap.lua error: " .. tostring(result), vim.log.levels.WARN)
          _project_dap_cache[root] = true
          _project_dap_loaded = true
          return
        end

        for ft, configs in pairs(result) do
          if type(configs) == "table" then
            dap.configurations[ft] = configs
            vim.notify("[dap] loaded project configs for " .. ft .. " from .nvim-dap.lua", vim.log.levels.INFO)
          end
        end

        _project_dap_cache[root] = true
        _project_dap_loaded = true
      end

      vim.api.nvim_create_autocmd({ "DirChanged" }, {
        group    = vim.api.nvim_create_augroup("DapProjectConfig", { clear = true }),
        callback = function()
          _project_dap_cache  = {}
          _project_dap_loaded = false
        end,
      })

      local orig_continue = dap.continue
      dap.continue = function(...)
        load_project_dap_config()   -- no-op after first call per cwd
        return orig_continue(...)
      end

      -- ── Deferred adapter setup ─────────────────────────────────────────────

      local function setup_java()
        dap.configurations.java = {
          { type = "java", request = "attach", name = "Debug (Attach)",
  hostName = "127.0.0.1",
  port = function() return tonumber(vim.fn.input("JDWP port [5005]: ")) or 5005 end },
          { type = "java", request = "launch", name = "Debug (Launch)", mainClass = "${file}" },
        }
      end

      local function setup_kotlin()
        dap.configurations.kotlin = {
          { type = "java", request = "attach", name = "Attach to Kotlin JVM", hostName = "127.0.0.1",
            port = function() return tonumber(vim.fn.input("JDWP port [5005]: ")) or 5005 end },
          { type = "java", request = "launch", name = "Launch Kotlin", mainClass = "${file}" },
        }
      end

      local function setup_codelldb()
        local codelldb_cmd = mason.bin("codelldb")
        if vim.fn.executable(codelldb_cmd) ~= 1 then
          vim.schedule(function() vim.notify("[dap] codelldb not found.\nRun: :MasonInstall codelldb", vim.log.levels.WARN) end)
          return
        end
        dap.adapters.codelldb = { type = "server", port = "${port}",
          executable = { command = codelldb_cmd, args = { "--port", "${port}" } } }
        local base = { name = "Launch file", type = "codelldb", request = "launch",
          program = function() return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file") end,
          cwd = "${workspaceFolder}", stopOnEntry = false }
        dap.configurations.c   = { vim.deepcopy(base) }
        dap.configurations.cpp = { vim.deepcopy(base) }
      end

      local function setup_go()
        register_adapter("delve", { "go" },
          function()
            local dlv = mason.bin("dlv")
            if vim.fn.executable(dlv) ~= 1 then return nil end
            return { type = "server", port = "${port}", executable = { command = dlv, args = { "dap", "-l", "127.0.0.1:${port}" } } }
          end,
          {
            { type = "delve", name = "Debug",               request = "launch", program = "${file}" },
            { type = "delve", name = "Debug test",          request = "launch", mode = "test", program = "${file}" },
            { type = "delve", name = "Debug test (go.mod)", request = "launch", mode = "test", program = "./${relativeFileDirname}" },
          },
          "[dap] delve not found.\nRun: :MasonInstall delve"
        )
      end

      -- ── js-debug-adapter path resolver ──────────────────────────────────────
      --
      local function find_js_debug_script()
        local path, is_alternate = mason.js_debug_script()
        if path and is_alternate then
          vim.notify("[dap] js-debug-adapter found at alternate path: " .. path, vim.log.levels.DEBUG)
        end
        return path
      end

      local function setup_js()
        local js_debug_script = find_js_debug_script()
        local node_bin        = vim.fn.exepath("node")

        if node_bin == "" then
          vim.schedule(function()
            vim.notify("[dap] node not found — JS/TS debugging unavailable", vim.log.levels.WARN)
          end)
          return
        end

        if not js_debug_script then
          vim.schedule(function()
            vim.notify(
              "[dap] js-debug-adapter not installed or path not found.\n"
              .. "Run: :MasonInstall js-debug-adapter\n"
              .. "If already installed, the Mason package layout may have changed.\n"
              .. "Check: " .. mason.packages_root() .. "/js-debug-adapter/",
              vim.log.levels.WARN
            )
          end)
          return
        end

        dap.adapters["pwa-node"] = { type = "server", host = "localhost", port = "${port}",
          executable = { command = node_bin, args = { js_debug_script, "${port}" } } }
        local js_launch = {
          { type = "pwa-node", request = "launch", name = "Launch file", program = "${file}", cwd = "${workspaceFolder}" },
          { type = "pwa-node", request = "attach", name = "Attach",
            processId = function() return require("dap.utils").pick_process() end, cwd = "${workspaceFolder}" },
        }
        dap.configurations.javascript      = js_launch
        dap.configurations.javascriptreact = js_launch
        local ts_launch = vim.deepcopy(js_launch)
        ts_launch[1] = vim.tbl_extend("force", ts_launch[1], { name = "Launch TS file", runtimeExecutable = "ts-node" })
        dap.configurations.typescript      = ts_launch
        dap.configurations.typescriptreact = ts_launch
      end

      local function setup_ruby()
        dap.adapters.ruby = function(callback, config)
          local use_bundle = vim.fn.executable("bundle") == 1
          local rdbg_bin   = (function()
            if vim.fn.executable("rdbg") == 1 then return vim.fn.exepath("rdbg") end
            local m = mason.bin("rdbg")
            if vim.fn.executable(m) == 1 then return m end
          end)()
          if not use_bundle and not rdbg_bin then
            vim.notify("[dap] Ruby debugger (rdbg) not found.\nRun: gem install rdbg", vim.log.levels.WARN)
            callback({ type = "server", host = "127.0.0.1", port = "${port}", executable = { command = "false", args = {} } })
            return
          end
          local args = use_bundle
            and { "exec", "rdbg", "-n", "--open", "--port", "${port}", "-c", "--", "ruby", config.program }
            or  { "-n", "--open", "--port", "${port}", "-c", "--", "ruby", config.program }
          callback({ type = "server", host = "127.0.0.1", port = "${port}",
            executable = { command = use_bundle and "bundle" or rdbg_bin, args = args } })
        end
        dap.configurations.ruby = {
          { type = "ruby", name = "Debug current file (bundle)", request = "attach", localfs = true, program = "${file}" },
          { type = "ruby", name = "Debug current file (rdbg)",   request = "attach", localfs = true, program = "${file}" },
        }
      end

      local function setup_elixir()
        register_adapter("mix_task", { "elixir" },
          function()
            local pkg_dbg    = mason.pkg("elixir-ls/debugger.sh")
            local standalone = vim.fn.exepath("elixir-ls-debugger")
            if vim.fn.filereadable(pkg_dbg) == 1 then return { type = "executable", command = pkg_dbg, args = {} } end
            if standalone ~= "" then return { type = "executable", command = standalone, args = {} } end
          end,
          {
            { type = "mix_task", name = "mix test", task = "test", taskArgs = { "--trace" }, request = "launch",
              startApps = true, projectDir = "${workspaceFolder}",
              requireFiles = { "test/**/test_helper.exs", "test/**/*_test.exs" } },
            { type = "mix_task", name = "mix phx.server", task = "phx.server", request = "launch", projectDir = "${workspaceFolder}" },
          },
          "[dap] Elixir DAP debugger not found.\nRun :MasonInstall elixir-ls"
        )
      end

      -- ── Deferred registration loop ─────────────────────────────────────────
      local deferred = {
        { pattern = "java",                                                           fn = setup_java,     suffix = "Java"    },
        { pattern = "kotlin",                                                         fn = setup_kotlin,   suffix = "Kotlin"  },
        { pattern = { "c","cpp","rust","zig" },                                       fn = setup_codelldb, suffix = "Codelldb"},
        { pattern = "go",                                                             fn = setup_go,       suffix = "Go"      },
        { pattern = { "javascript","typescript","javascriptreact","typescriptreact"}, fn = setup_js,       suffix = "Js"      },
        { pattern = "ruby",                                                           fn = setup_ruby,     suffix = "Ruby"    },
        { pattern = "elixir",                                                         fn = setup_elixir,   suffix = "Elixir"  },
      }
      local function ft_wanted(ft, pattern)
        if type(pattern) == "table" then return vim.tbl_contains(pattern, ft) end
        return ft == pattern
      end

      for _, entry in ipairs(deferred) do
        local done = false
        local function run()
          if done then return end
          done = true
          local ok, err = pcall(entry.fn)
          if not ok then
            vim.notify(string.format("[dap] %s adapter setup failed: %s", entry.suffix, tostring(err)), vim.log.levels.WARN)
          end
        end

        vim.api.nvim_create_autocmd("FileType", {
          pattern  = entry.pattern, once = true,
          group    = vim.api.nvim_create_augroup("DapDeferred" .. entry.suffix, { clear = true }),
          callback = run, desc = "Register DAP adapter: " .. entry.suffix,
        })

        -- nvim-dap loads on first use — normally AFTER the relevant buffer's FileType
        -- event already fired — so also cover buffers that are already open.
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          if vim.api.nvim_buf_is_loaded(buf) and ft_wanted(vim.bo[buf].filetype, entry.pattern) then
            run(); break
          end
        end
      end

      -- ── Persistent breakpoints ─────────────────────────────────────────────
      local function get_bp_file()
        local ok_path, path_util = pcall(require, "core.util.path")
        local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
        local hash = vim.fn.sha256(root):sub(1, 16)
        local dir  = vim.fn.stdpath("data") .. "/dap-breakpoints"
        if vim.fn.isdirectory(dir) ~= 1 then vim.fn.mkdir(dir, "p") end
        return dir .. "/" .. hash .. ".json"
      end

      local function serialize_breakpoints(bp_data)
        local bps = {}
        for bufnr, buf_bps in pairs(bp_data) do
          local ok_name, path = pcall(vim.api.nvim_buf_get_name, bufnr)
          if not ok_name then path = "" end
          if path ~= "" and #buf_bps > 0 then
            bps[path] = vim.tbl_map(function(bp)
              return { line = bp.line, condition = bp.condition, log_message = bp.log_message }
            end, buf_bps)
          end
        end
        return bps
      end

      local function save_breakpoints()
        local ok_bp, bp_data = pcall(function() return require("dap.breakpoints").get() end)
        if not ok_bp then return end
        local bps     = serialize_breakpoints(bp_data)
        local bp_file = get_bp_file()
        if next(bps) == nil then
          pcall(os.remove, bp_file)
          return
        end
        pcall(vim.fn.writefile, { vim.json.encode(bps) }, bp_file)
      end

      local function set_bps_scheduled(bufnr, entries)
        vim.schedule(function()
          if vim.b[bufnr] and vim.b[bufnr].large_file then return end
          for _, bp in ipairs(entries) do
            pcall(function()
              require("dap.breakpoints").set(
                { condition = bp.condition, log_message = bp.log_message },
                bufnr, math.floor(bp.line))
            end)
          end
        end)
      end

      local function load_breakpoints()
        local ok_read, lines = pcall(vim.fn.readfile, get_bp_file())
        if not ok_read or not lines or #lines == 0 then return end
        local ok_j, bps = pcall(vim.json.decode, table.concat(lines, "\n"))
        if not ok_j or not bps then return end
        for path, entries in pairs(bps) do
          local bufnr = vim.fn.bufnr(path)
          if bufnr ~= -1 then
            set_bps_scheduled(bufnr, entries)
          else
            local aug = vim.api.nvim_create_augroup("DapBpRestore_" .. vim.fn.sha256(path):sub(1, 32), { clear = true })
            vim.api.nvim_create_autocmd("BufReadPost", {
              pattern = "*", group = aug,
              callback = function(e)
                if vim.api.nvim_buf_get_name(e.buf) ~= path then return end
                set_bps_scheduled(e.buf, entries)
                pcall(vim.api.nvim_del_augroup_id, aug)
              end,
            })
          end
        end
      end

      -- Save on clean exit
      vim.api.nvim_create_autocmd("VimLeavePre", {
        group    = vim.api.nvim_create_augroup("DapBreakpointsSave", { clear = true }),
        callback = save_breakpoints,
      })

      local _bp_autosave_timer = nil
      local autosave_ms = (type(vim.g.dap_bp_autosave_ms) == "number" and vim.g.dap_bp_autosave_ms > 0)
        and vim.g.dap_bp_autosave_ms or 60000

      local function start_bp_autosave()
        if _bp_autosave_timer then return end
        _bp_autosave_timer = vim.uv.new_timer()
        _bp_autosave_timer:start(autosave_ms, autosave_ms, vim.schedule_wrap(function()
          save_breakpoints()
        end))
      end

      dap.listeners.after.event_initialized["bp_autosave"] = function()
        start_bp_autosave()
      end
      local function stop_bp_autosave()
        if _bp_autosave_timer then
          pcall(function() _bp_autosave_timer:stop(); _bp_autosave_timer:close() end)
          _bp_autosave_timer = nil
        end
      end
      dap.listeners.before.event_terminated["bp_autosave"] = stop_bp_autosave
      dap.listeners.before.event_exited["bp_autosave"]     = stop_bp_autosave

      vim.api.nvim_create_autocmd("VimLeavePre", {
        group    = vim.api.nvim_create_augroup("DapBpAutosaveStop", { clear = true }),
        callback = function()
          if _bp_autosave_timer then
            pcall(function() _bp_autosave_timer:stop(); _bp_autosave_timer:close() end)
            _bp_autosave_timer = nil
          end
        end,
      })

      vim.schedule(load_breakpoints)
      vim.api.nvim_create_autocmd("SessionLoadPost", {
        group    = vim.api.nvim_create_augroup("DapBreakpointsLoad", { clear = true }),
        callback = load_breakpoints,
      })
    end,
  },

  { "rcarriga/nvim-dap-virtual-text",
    dependencies = "mfussenegger/nvim-dap", event = "VeryLazy",
    opts = { enabled = true, highlight_changed_variables = true, highlight_new_as_changed = true, show_stop_reason = true, commented = false },
    config = function(_, opts) pcall(function() require("nvim-dap-virtual-text").setup(opts) end) end },
}

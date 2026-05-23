-- lua/plugins/specs/dap.lua — Debug Adapter Protocol
--

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-neotest/nvim-nio",
    },
    keys = {
      { "<F5>" }, { "<F6>" }, { "<F7>" }, { "<F8>" }, { "<F9>" },
      { "<F10>" }, { "<F11>" },
      { "<leader>;c" }, { "<leader>;b" },
    },
    config = function()
      local dap   = require("dap")
      local dapui = require("dapui")
      local mason = require("core.util.mason")

      -- ── DAP UI ──────────────────────────────────────────────────────────────
      pcall(function()
        dapui.setup({
          icons    = { expanded = "▾", collapsed = "▸", current_frame = "▸" },
          layouts  = {
            {
              elements = {
                { id = "scopes",      size = 0.25 },
                { id = "breakpoints", size = 0.25 },
                { id = "stacks",      size = 0.25 },
                { id = "watches",     size = 0.25 },
              },
              size = 40, position = "left",
            },
            {
              elements = {
                { id = "repl",    size = 0.5 },
                { id = "console", size = 0.5 },
              },
              size = 10, position = "bottom",
            },
          },
          controls = {
            enabled = true,
            element = "repl",
            icons   = {
              pause     = "",
              play      = "",
              step_into = "",
              step_over = "",
              step_out  = "",
              step_back = "",
              run_last  = "▶▶",
              terminate = "⏹",
            },
          },
          floating = { max_width = 0.9, max_height = 0.5, border = "rounded" },
        })
      end)

      -- ── Signs ────────────────────────────────────────────────────────────────
      local signs = {
        DapBreakpoint          = { text = "●", texthl = "DapBreakpoint",  linehl = "",              numhl = "" },
        DapStopped             = { text = "▶", texthl = "DapStopped",     linehl = "DapStoppedLine", numhl = "" },
        DapBreakpointCondition = { text = "◆", texthl = "DapBreakpoint",  linehl = "",              numhl = "" },
        DapBreakpointRejected  = { text = "✖", texthl = "DapBreakpoint",  linehl = "",              numhl = "" },
        DapLogPoint            = { text = "◉", texthl = "DapLogPoint",    linehl = "",              numhl = "" },
      }
      for sign, cfg in pairs(signs) do
        pcall(vim.fn.sign_define, sign, cfg)
      end

      -- ── Listeners ────────────────────────────────────────────────────────────
      local function safe_open()  pcall(function() dapui.open()  end) end
      local function safe_close() pcall(function() dapui.close() end) end
      dap.listeners.after.event_initialized["dapui_config"]  = safe_open
      dap.listeners.before.event_terminated["dapui_config"]  = safe_close
      dap.listeners.before.event_exited["dapui_config"]      = safe_close

      -- ── register_adapter helper ──────────────────────────────────────────────
      local function register_adapter(name, check_fn, configs, warn_msg)
        local adapter = check_fn()
        if adapter then
          dap.adapters[name]       = adapter
          dap.configurations[name] = configs
        else
          vim.schedule(function()
            vim.notify(warn_msg, vim.log.levels.WARN)
          end)
        end
      end

      -- ── Deferred adapter setup functions ─────────────────────────────────────

      local function setup_java()
        dap.configurations.java = {
          { type = "java", request = "attach", name = "Debug (Attach) - Remote",
            hostName = "127.0.0.1", port = 5005 },
          { type = "java", request = "launch", name = "Debug (Launch) - Current File",
            mainClass = "${file}" },
        }
      end

      local function setup_kotlin()
        dap.configurations.kotlin = {
          {
            type     = "java", request = "attach",
            name     = "Attach to Kotlin JVM",
            hostName = "127.0.0.1",
            port     = function()
              return tonumber(vim.fn.input("JDWP port [5005]: ")) or 5005
            end,
          },
          {
            type      = "java", request = "launch",
            name      = "Launch Kotlin (current file)",
            mainClass = "${file}",
          },
        }
      end

      local function setup_codelldb()
        local codelldb_cmd = mason.bin("codelldb")
        if vim.fn.executable(codelldb_cmd) ~= 1 then
          vim.notify(
            "[dap] codelldb not found — C / C++ / Rust debug unavailable.\n"
            .. "Run: :MasonInstall codelldb",
            vim.log.levels.WARN
          )
        end

        dap.adapters.codelldb = {
          type = "server", port = "${port}",
          executable = { command = codelldb_cmd, args = { "--port", "${port}" } },
        }

        local base_launch = {
          name    = "Launch file",
          type    = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}", stopOnEntry = false,
        }

        dap.configurations.c   = { vim.deepcopy(base_launch) }
        dap.configurations.cpp = { vim.deepcopy(base_launch) }

      end

      local function setup_go()
        register_adapter(
          "delve",
          function()
            local dlv = mason.bin("dlv")
            if vim.fn.executable(dlv) ~= 1 then return nil end
            return {
              type = "server", port = "${port}",
              executable = {
                command = dlv,
                args    = { "dap", "-l", "127.0.0.1:${port}" },
              },
            }
          end,
          {
            { type = "delve", name = "Debug",               request = "launch", program = "${file}" },
            { type = "delve", name = "Debug test",          request = "launch", mode = "test", program = "${file}" },
            { type = "delve", name = "Debug test (go.mod)", request = "launch", mode = "test",
              program = "./${relativeFileDirname}" },
          },
          "[dap] delve not found — Go debug unavailable.\nRun: :MasonInstall delve"
        )
      end

      local function setup_js()
        local js_debug_script = mason.pkg("js-debug-adapter/js-debug/src/dapDebugServer.js")
        local node_bin        = vim.fn.exepath("node")

        if node_bin == "" or vim.fn.filereadable(js_debug_script) ~= 1 then
          vim.schedule(function()
            local msg = node_bin == ""
              and "[dap] node not found — JS/TS debug unavailable"
              or  "[dap] js-debug-adapter not installed — run :MasonInstall js-debug-adapter"
            vim.notify(msg, vim.log.levels.WARN)
          end)
          return
        end

        dap.adapters["pwa-node"] = {
          type = "server", host = "localhost", port = "${port}",
          executable = {
            command = node_bin,
            args    = { js_debug_script, "${port}" },
          },
        }

        local js_launch = {
          { type = "pwa-node", request = "launch", name = "Launch file",
            program = "${file}", cwd = "${workspaceFolder}" },
          { type = "pwa-node", request = "attach", name = "Attach",
            processId = function()
              return require("dap.utils").pick_process()
            end,
            cwd = "${workspaceFolder}" },
        }

        dap.configurations.javascript      = js_launch
        dap.configurations.javascriptreact = js_launch

        local ts_launch = vim.deepcopy(js_launch)
        ts_launch[1] = vim.tbl_extend("force", ts_launch[1], {
          name              = "Launch TS file",
          runtimeExecutable = "ts-node",
        })
        dap.configurations.typescript      = ts_launch
        dap.configurations.typescriptreact = ts_launch
      end

      local function setup_ruby()
        local use_bundle = vim.fn.executable("bundle") == 1
        local rdbg_bin   = (function()
          if vim.fn.executable("rdbg") == 1 then return vim.fn.exepath("rdbg") end
          local m = mason.bin("rdbg")
          if vim.fn.executable(m) == 1 then return m end
          return nil
        end)()

        if not use_bundle and not rdbg_bin then
          vim.notify(
            "[dap] Ruby debugger (rdbg) not found.\n"
            .. "Run: gem install rdbg  OR  bundle add rdbg",
            vim.log.levels.WARN
          )
          return
        end

        dap.adapters.ruby = function(callback, config)
          local args = use_bundle
            and { "exec", "rdbg", "-n", "--open", "--port", "${port}",
                  "-c", "--", "ruby", config.program }
            or  { "-n", "--open", "--port", "${port}",
                  "-c", "--", "ruby", config.program }
          callback({
            type = "server", host = "127.0.0.1", port = "${port}",
            executable = {
              command = use_bundle and "bundle" or rdbg_bin,
              args    = args,
            },
          })
        end

        dap.configurations.ruby = {
          { type = "ruby", name = "Debug current file (bundle)", request = "attach",
            localfs = true, program = "${file}" },
          { type = "ruby", name = "Debug current file (rdbg)",   request = "attach",
            localfs = true, program = "${file}" },
        }
      end

      local function setup_elixir()
        register_adapter(
          "mix_task",
          function()
            local pkg_dbg    = mason.pkg("elixir-ls/debugger.sh")
            local standalone = vim.fn.exepath("elixir-ls-debugger")
            if vim.fn.filereadable(pkg_dbg) == 1 then
              return { type = "executable", command = pkg_dbg,    args = {} }
            end
            if standalone ~= "" then
              return { type = "executable", command = standalone, args = {} }
            end
            return nil
          end,
          {
            { type = "mix_task", name = "mix test", task = "test",
              taskArgs = { "--trace" }, request = "launch", startApps = true,
              projectDir = "${workspaceFolder}",
              requireFiles = { "test/**/test_helper.exs", "test/**/*_test.exs" } },
            { type = "mix_task", name = "mix phx.server", task = "phx.server",
              request = "launch", projectDir = "${workspaceFolder}" },
          },
          "[dap] Elixir DAP debugger not found.\nRun :MasonInstall elixir-ls"
        )
      end

      -- ── Deferred registration loop ───────────────────────────────────────────

      local deferred = {
        { pattern = "java",                                                     fn = setup_java,     suffix = "Java"     },
        { pattern = "kotlin",                                                   fn = setup_kotlin,   suffix = "Kotlin"   },
        { pattern = { "c", "cpp", "rust" },                                     fn = setup_codelldb, suffix = "Codelldb"  },
        { pattern = "go",                                                        fn = setup_go,       suffix = "Go"       },
        { pattern = { "javascript","typescript","javascriptreact","typescriptreact" }, fn = setup_js, suffix = "Js"       },
        { pattern = "ruby",                                                      fn = setup_ruby,     suffix = "Ruby"     },
        { pattern = "elixir",                                                    fn = setup_elixir,   suffix = "Elixir"   },
      }

      for _, entry in ipairs(deferred) do
        vim.api.nvim_create_autocmd("FileType", {
          pattern  = entry.pattern,
          once     = true,
          group    = vim.api.nvim_create_augroup("DapDeferred" .. entry.suffix, { clear = true }),
          callback = entry.fn,
          desc     = "Register DAP adapter: " .. entry.suffix,
        })
      end

      -- ── Persistent breakpoints ────────────────────────────────────────────────
      local bp_file = vim.fn.stdpath("data") .. "/dap-breakpoints.json"

      local function serialize_breakpoints(bp_data)
        local bps = {}
        for bufnr, buf_bps in pairs(bp_data) do
          local path = vim.api.nvim_buf_get_name(bufnr)
          if path ~= "" and #buf_bps > 0 then
            bps[path] = vim.tbl_map(function(bp)
              return {
                line        = bp.line,
                condition   = bp.condition,
                log_message = bp.log_message,
              }
            end, buf_bps)
          end
        end
        return bps
      end

      local function save_breakpoints()
        local ok_bp, bp_data = pcall(function() return require("dap.breakpoints").get() end)
        if not ok_bp then return end
        local bps = serialize_breakpoints(bp_data)
        if next(bps) == nil then return end
        pcall(vim.fn.writefile, { vim.json.encode(bps) }, bp_file)
      end

      local function set_bps_scheduled(bufnr, entries)
        vim.schedule(function()
          if vim.b[bufnr] and vim.b[bufnr].large_file then
            vim.notify(
              string.format("[dap] Skipping breakpoint restore for large file buffer %d", bufnr),
              vim.log.levels.WARN
            )
            return
          end
          for _, bp in ipairs(entries) do
            pcall(function()
              require("dap.breakpoints").set(
                { condition = bp.condition, log_message = bp.log_message },
                bufnr,
                math.floor(bp.line)
              )
            end)
          end
        end)
      end

      local function load_breakpoints()
        local ok_read, lines = pcall(vim.fn.readfile, bp_file)
        if not ok_read or not lines or #lines == 0 then return end

        local ok_j, bps = pcall(vim.json.decode, table.concat(lines, "\n"))
        if not ok_j or not bps then
          vim.notify(
            "[dap] Could not parse breakpoints file — it may be corrupt.\n"
            .. "Delete it to start fresh: " .. bp_file,
            vim.log.levels.WARN
          )
          return
        end

        for path, entries in pairs(bps) do
          local bufnr = vim.fn.bufnr(path)
          if bufnr ~= -1 then
            set_bps_scheduled(bufnr, entries)
          else
            local aug = vim.api.nvim_create_augroup(
              "DapBpRestore_" .. vim.fn.sha256(path):sub(1, 16), { clear = true }
            )
            vim.api.nvim_create_autocmd("BufReadPost", {
              pattern  = "*",
              group    = aug,
              callback = function(e)
                if vim.api.nvim_buf_get_name(e.buf) ~= path then return end
                set_bps_scheduled(e.buf, entries)
                pcall(vim.api.nvim_del_augroup_by_id, aug)
              end,
            })
          end
        end
      end

      vim.api.nvim_create_autocmd("VimLeavePre", {
        group    = vim.api.nvim_create_augroup("DapBreakpointsSave", { clear = true }),
        callback = save_breakpoints,
      })

      local load_group = vim.api.nvim_create_augroup("DapBreakpointsLoad", { clear = true })
      vim.api.nvim_create_autocmd("User", {
        pattern  = "LazyDone",
        once     = true,
        group    = load_group,
        callback = load_breakpoints,
      })
      vim.api.nvim_create_autocmd("SessionLoadPost", {
        once     = true,
        group    = load_group,
        callback = load_breakpoints,
      })
    end,
  },

  -- ── mason-nvim-dap ───────────────────────────────────────────────────────────

  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason.nvim", "nvim-dap" },
    opts = {
      ensure_installed    = require("core.util.packages").mason.dap,
      automatic_installation = true,
    },
  },

  {
    "rcarriga/nvim-dap-virtual-text",
    dependencies = "mfussenegger/nvim-dap",
    event = "VeryLazy",
    opts = {
      enabled                     = true,
      highlight_changed_variables = true,
      highlight_new_as_changed    = true,
      show_stop_reason            = true,
      commented                   = false,
    },
    config = function(_, opts)
      pcall(function() require("nvim-dap-virtual-text").setup(opts) end)
    end,
  },
}

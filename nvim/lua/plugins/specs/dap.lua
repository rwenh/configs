-- lua/plugins/specs/dap.lua - Debug Adapter Protocol
--
-- FIX (v2.3.1):
--   • load_breakpoints() wraps dap.breakpoints.set() in vim.schedule().
--
-- FIX (v2.3.2):
--   • mason-nvim-dap handlers={} restores default adapter setup.
--
-- FIX (v2.3.3):
--   • load_breakpoints() was called via vim.defer_fn(fn, 100) on VimEnter.
--     100ms is fragile — on fast machines it fires before lazy.nvim finishes
--     loading nvim-dap, so require("dap.breakpoints") fails silently. Fixed:
--     hook into the "User LazyDone" event instead, which fires only after ALL
--     plugins are fully initialised. No arbitrary timer needed.
--   • JS/TS dap config: require("dap.utils").pick_process was called eagerly
--     at config time (not inside a function). If nvim-dap hadn't finished
--     loading yet this would error. Wrapped in a function so it's evaluated
--     lazily at debug-session start time.

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

      -- ── DAP UI setup ─────────────────────────────────────────────────
      pcall(function()
        dapui.setup({
          icons = { expanded = "▾", collapsed = "▸", current_frame = "▸" },
          layouts = {
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
            icons = {
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

      local function safe_open()  pcall(function() dapui.open()  end) end
      local function safe_close() pcall(function() dapui.close() end) end
      dap.listeners.after.event_initialized["dapui_config"]  = safe_open
      dap.listeners.before.event_terminated["dapui_config"]  = safe_close
      dap.listeners.before.event_exited["dapui_config"]      = safe_close

      -- ── Shared helpers ───────────────────────────────────────────────
      local function mason_bin(name)
        local p = vim.fn.exepath(name)
        if p ~= "" then return p end
        return vim.fn.stdpath("data") .. "/mason/bin/" .. name
      end

      local function mason_pkg(rel)
        return vim.fn.stdpath("data") .. "/mason/packages/" .. rel
      end

      -- ── Python ──────────────────────────────────────────────────────
      local function find_python()
        local p3 = vim.fn.exepath("python3")
        if p3 ~= "" then return p3 end
        local p = vim.fn.exepath("python")
        if p ~= "" then return p end
        return "python3"
      end

      dap.adapters.python = function(cb, config)
        if config.request == "attach" then
          local port = (config.connect or config).port
          local host = (config.connect or config).host or "127.0.0.1"
          cb({ type = "server", port = assert(port, "`connect.port` required"), host = host,
               options = { source_filetype = "python" } })
        else
          cb({ type = "executable", command = find_python(),
               args = { "-m", "debugpy.adapter" },
               options = { source_filetype = "python" } })
        end
      end

      dap.configurations.python = {
        { type = "python", request = "launch", name = "Launch file", program = "${file}",
          pythonPath = function()
            local v = os.getenv("VIRTUAL_ENV")
            return v and (v .. "/bin/python") or find_python()
          end },
        { type = "python", request = "launch", name = "Launch file with arguments", program = "${file}",
          args = function() return vim.split(vim.fn.input("Arguments: "), " +") end,
          pythonPath = function()
            local v = os.getenv("VIRTUAL_ENV")
            return v and (v .. "/bin/python") or find_python()
          end },
        { type = "python", request = "attach", name = "Attach remote",
          connect = function()
            local host = vim.fn.input("Host [127.0.0.1]: ")
            host = host ~= "" and host or "127.0.0.1"
            local port = tonumber(vim.fn.input("Port [5678]: ")) or 5678
            return { host = host, port = port }
          end },
      }

      -- ── Java ─────────────────────────────────────────────────────────
      dap.configurations.java = {
        { type = "java", request = "attach", name = "Debug (Attach) - Remote",
          hostName = "127.0.0.1", port = 5005 },
        { type = "java", request = "launch", name = "Debug (Launch) - Current File",
          mainClass = "${file}" },
      }

      -- ── Kotlin (via jdtls + java-debug-adapter) ───────────────────
      dap.configurations.kotlin = {
        {
          type      = "java",
          request   = "attach",
          name      = "Attach to Kotlin JVM",
          hostName  = "127.0.0.1",
          port      = function()
            return tonumber(vim.fn.input("JDWP port [5005]: ")) or 5005
          end,
        },
        {
          type      = "java",
          request   = "launch",
          name      = "Launch Kotlin (current file)",
          mainClass = "${file}",
        },
      }

      -- ── C / C++ / Rust (codelldb) ─────────────────────────────────
      dap.adapters.codelldb = {
        type = "server", port = "${port}",
        executable = {
          command = mason_bin("codelldb"),
          args    = { "--port", "${port}" },
        },
      }

      local codelldb_launch = {
        name    = "Launch file",
        type    = "codelldb",
        request = "launch",
        program = function()
          return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
        end,
        cwd = "${workspaceFolder}", stopOnEntry = false,
      }

      dap.configurations.c   = { vim.deepcopy(codelldb_launch) }
      dap.configurations.cpp = { vim.deepcopy(codelldb_launch) }
      dap.configurations.rust = {
        vim.tbl_extend("force", vim.deepcopy(codelldb_launch), {
          name    = "Launch file",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/target/debug/", "file")
          end,
        }),
      }

      -- ── Go (delve) ────────────────────────────────────────────────
      dap.adapters.delve = {
        type = "server", port = "${port}",
        executable = {
          command = mason_bin("dlv"),
          args    = { "dap", "-l", "127.0.0.1:${port}" },
        },
      }
      dap.configurations.go = {
        { type = "delve", name = "Debug",               request = "launch", program = "${file}" },
        { type = "delve", name = "Debug test",          request = "launch", mode = "test", program = "${file}" },
        { type = "delve", name = "Debug test (go.mod)", request = "launch", mode = "test",
          program = "./${relativeFileDirname}" },
      }

      -- ── JavaScript / TypeScript (pwa-node) ───────────────────────
      local js_debug_script = mason_pkg("js-debug-adapter/js-debug/src/dapDebugServer.js")
      local node_bin = vim.fn.exepath("node")

      if node_bin ~= "" and vim.fn.filereadable(js_debug_script) == 1 then
        dap.adapters["pwa-node"] = {
          type = "server", host = "localhost", port = "${port}",
          executable = {
            command = node_bin,
            args    = { js_debug_script, "${port}" },
          },
        }
        dap.configurations.javascript = {
          { type = "pwa-node", request = "launch", name = "Launch file",
            program = "${file}", cwd = "${workspaceFolder}" },
          { type = "pwa-node", request = "attach", name = "Attach",
            -- FIX: was require("dap.utils").pick_process (eager, at config time).
            -- Wrapped in a function so it is evaluated lazily at session start,
            -- after nvim-dap is guaranteed to be fully loaded.
            processId = function()
              return require("dap.utils").pick_process()
            end,
            cwd = "${workspaceFolder}" },
        }
        dap.configurations.typescript = {
          { type = "pwa-node", request = "launch", name = "Launch file",
            program = "${file}", cwd = "${workspaceFolder}",
            runtimeExecutable = "ts-node" },
        }
        dap.configurations.javascriptreact = dap.configurations.javascript
        dap.configurations.typescriptreact = dap.configurations.typescript
      else
        vim.schedule(function()
          if node_bin == "" then
            vim.notify("[dap] node not found — JS/TS debug unavailable", vim.log.levels.WARN)
          else
            vim.notify("[dap] js-debug-adapter not installed — run :MasonInstall js-debug-adapter",
              vim.log.levels.WARN)
          end
        end)
      end

      -- ── Ruby (rdbg) ───────────────────────────────────────────────
      dap.adapters.ruby = function(callback, config)
        callback({
          type = "server", host = "127.0.0.1", port = "${port}",
          executable = {
            command = "bundle",
            args    = { "exec", "rdbg", "-n", "--open", "--port", "${port}",
                        "-c", "--", "ruby", config.program },
          },
        })
      end
      dap.configurations.ruby = {
        { type = "ruby", name = "Debug current file", request = "attach",
          localfs = true, program = "${file}" },
      }

      -- ── Elixir (ElixirLS debugger) ────────────────────────────────
      local elixir_dbg = (function()
        local pkg_bin = mason_pkg("elixir-ls/debugger.sh")
        if vim.fn.filereadable(pkg_bin) == 1 then return pkg_bin end
        local a = vim.fn.exepath("elixir-ls-debugger")
        if a ~= "" then return a end
        local b = vim.fn.exepath("elixir-ls")
        if b ~= "" then return b end
        return mason_bin("elixir-ls")
      end)()
      dap.adapters.mix_task = {
        type    = "executable",
        command = elixir_dbg,
        args    = {},
      }
      dap.configurations.elixir = {
        { type = "mix_task", name = "mix test", task = "test",
          taskArgs = { "--trace" }, request = "launch", startApps = true,
          projectDir = "${workspaceFolder}",
          requireFiles = { "test/**/test_helper.exs", "test/**/*_test.exs" } },
        { type = "mix_task", name = "mix phx.server", task = "phx.server",
          request = "launch", projectDir = "${workspaceFolder}" },
      }

      -- ── Persistent breakpoints ────────────────────────────────────
      local bp_file = vim.fn.stdpath("data") .. "/dap-breakpoints.json"

      local function save_breakpoints()
        local bps = {}
        local ok_bp, bp_data = pcall(function() return require("dap.breakpoints").get() end)
        if not ok_bp then return end

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
        if next(bps) == nil then return end

        local f = io.open(bp_file, "w")
        if f then
          f:write(vim.json.encode(bps))
          f:close()
        end
      end

      local function set_bps_scheduled(bufnr, entries)
        vim.schedule(function()
          for _, bp in ipairs(entries) do
            pcall(function()
              require("dap.breakpoints").set(
                { condition = bp.condition, log_message = bp.log_message },
                bufnr, bp.line)
            end)
          end
        end)
      end

      local function load_breakpoints()
        local f = io.open(bp_file, "r")
        if not f then return end
        local ok_j, bps = pcall(vim.json.decode, f:read("*a"))
        f:close()
        if not ok_j or not bps then return end

        for path, entries in pairs(bps) do
          local bufnr = vim.fn.bufnr(path)
          if bufnr ~= -1 then
            set_bps_scheduled(bufnr, entries)
          else
            local aug = vim.api.nvim_create_augroup(
              "DapBpRestore_" .. vim.fn.sha256(path):sub(1, 8), { clear = true }
            )
            vim.api.nvim_create_autocmd("BufReadPost", {
              pattern  = "*",
              group    = aug,
              callback = function(e)
                local evfile = vim.api.nvim_buf_get_name(e.buf)
                if evfile ~= path then return end
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

      -- FIX: was vim.defer_fn(load_breakpoints, 100) on VimEnter.
      -- 100ms is fragile — on fast machines nvim-dap may not be fully
      -- initialised yet, causing require("dap.breakpoints") to fail silently.
      -- "User LazyDone" fires only after every plugin's config() has run,
      -- so nvim-dap is guaranteed ready at that point.
      vim.api.nvim_create_autocmd("User", {
        pattern  = "LazyDone",
        once     = true,
        group    = vim.api.nvim_create_augroup("DapBreakpointsLoad", { clear = true }),
        callback = load_breakpoints,
      })
    end,
  },

  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason.nvim", "nvim-dap" },
    opts = {
      ensure_installed       = { "python", "codelldb", "delve", "js-debug-adapter", "java-debug-adapter", "java-test" },
      automatic_installation = true,
      handlers               = {},
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
      require("nvim-dap-virtual-text").setup(opts)
    end,
  },
}

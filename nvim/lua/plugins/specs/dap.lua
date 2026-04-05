-- lua/plugins/specs/dap.lua - Debug Adapter Protocol
--
-- FIX (v2.2.4):
--   • load_breakpoints(): BufReadPost autocmd used pattern=path (absolute path
--     literal). vim.api.nvim_create_autocmd pattern matching uses fnamemodify
--     patterns, not literal paths — a path like "/home/user/project/main.lua"
--     is tested against the autocmd pattern glob. An absolute path as a pattern
--     matches ONLY that exact file via fnmatch, which actually works on most
--     systems but is fragile and undocumented. Changed to pass the path as a
--     once=true callback that checks the event's file against the target path
--     explicitly — reliable on all platforms including Windows paths with spaces.
--   • mason-nvim-dap handlers={} overrides automatic_installation by providing
--     an empty handler table, which tells mason-nvim-dap "I handle everything"
--     and disables the default auto-install handler. Changed to handlers=nil so
--     the default handler runs and automatic_installation actually works.
--   • save_breakpoints(): bp.logMessage alias removed (was already fixed in
--     v2.2.3). bp.log_message is the sole field used.
--   • nvim-dap-ui standalone spec removed (duplicate setup() call).
--   • Kotlin DAP uses java-debug-adapter via jdtls (JDWP). No separate adapter.

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
            processId = require("dap.utils").pick_process,
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
      local elixir_dbg = vim.fn.exepath("elixir-ls-debugger")
      dap.adapters.mix_task = {
        type    = "executable",
        command = elixir_dbg ~= "" and elixir_dbg
                  or mason_bin("elixir-ls-debugger"),
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

      local function load_breakpoints()
        local f = io.open(bp_file, "r")
        if not f then return end
        local ok_j, bps = pcall(vim.json.decode, f:read("*a"))
        f:close()
        if not ok_j or not bps then return end

        for path, entries in pairs(bps) do
          local bufnr = vim.fn.bufnr(path)
          if bufnr ~= -1 then
            for _, bp in ipairs(entries) do
              pcall(function()
                require("dap.breakpoints").set(
                  { condition = bp.condition, log_message = bp.log_message },
                  bufnr, bp.line)
              end)
            end
          else
            -- FIX: rather than using the absolute path as the autocmd pattern
            -- (which relies on undocumented glob matching behaviour and breaks
            -- on Windows-style paths), we register a once=true BufReadPost
            -- autocmd with pattern="*" and check the event's file explicitly.
            -- This is portable and unambiguous on all platforms.
            vim.api.nvim_create_autocmd("BufReadPost", {
              pattern  = "*",
              once     = true,
              callback = function(e)
                local evfile = vim.api.nvim_buf_get_name(e.buf)
                if evfile ~= path then return end
                for _, bp in ipairs(entries) do
                  pcall(function()
                    require("dap.breakpoints").set(
                      { condition = bp.condition, log_message = bp.log_message },
                      e.buf, bp.line)
                  end)
                end
              end,
            })
          end
        end
      end

      vim.api.nvim_create_autocmd("VimLeavePre", {
        group    = vim.api.nvim_create_augroup("DapBreakpointsSave", { clear = true }),
        callback = save_breakpoints,
      })
      vim.api.nvim_create_autocmd("VimEnter", {
        group    = vim.api.nvim_create_augroup("DapBreakpointsLoad", { clear = true }),
        callback = function() vim.defer_fn(load_breakpoints, 100) end,
      })
    end,
  },

  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason.nvim", "nvim-dap" },
    opts = {
      ensure_installed       = { "python", "codelldb", "delve", "js-debug-adapter", "java-debug-adapter", "java-test" },
      automatic_installation = true,
      -- FIX: handlers={} overrides automatic_installation by replacing the
      -- default handler with an empty table — mason-nvim-dap interprets this
      -- as "caller handles all adapters" and skips auto-install entirely.
      -- handlers=nil preserves the built-in default handler so
      -- automatic_installation actually installs missing adapters.
      handlers = nil,
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

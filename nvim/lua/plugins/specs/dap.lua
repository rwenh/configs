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
--
-- FIX (v2.3.4):
--   • mason-nvim-dap: handlers={} was incorrect. An empty table suppresses ALL
--     default adapter handlers — no adapters get auto-configured. The correct
--     form to get mason-nvim-dap's built-in default handler for every installed
--     adapter is to omit the handlers key entirely (or set it to nil). Removed.
--
-- FIX (v2.3.6):
--   • mason-nvim-dap ensure_installed listed "python" — the Mason registry
--     package name is "debugpy". The wrong name caused mason-nvim-dap to warn
--     on every startup that the package was not found, and the Python DAP
--     adapter was never auto-installed. This was a known issue since v2.2.4;
--     the fix is a single token change.
--
-- FIX (v2.3.9):
--   • Elixir DAP debugger path resolver: the previous resolver checked
--     mason_pkg("elixir-ls/debugger.sh") first, then fell through to
--     exepath("elixir-ls") — the LSP binary — as a last resort. The LSP
--     binary is NOT the DAP debugger; using it causes elixir DAP sessions
--     to fail immediately. The fallback chain now only returns an executable
--     that is actually a DAP-capable binary (debugger.sh or elixir-ls-debugger).
--     If none is found the adapter is set to nil and a deferred warning is
--     shown rather than silently wiring the wrong binary. The mix_task adapter
--     registration is now guarded so it only fires when a valid debugger is
--     found.

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
      -- FIX (v2.3.9): The old resolver fell through to exepath("elixir-ls")
      -- as a last resort. That path resolves to the LSP binary, not the DAP
      -- debugger — wiring the LSP binary as a DAP adapter silently fails every
      -- debug session. The resolver now only returns a path that is known to be
      -- a DAP-capable binary. If no valid debugger is found, the adapter is not
      -- registered and a warning is shown. Known good paths in order of priority:
      --   1. Mason package debugger.sh  (elixir-ls bundle, most reliable)
      --   2. elixir-ls-debugger         (standalone binary, some distros)
      --   3. nil                        (warn and skip — do NOT use elixir-ls)
      local elixir_dbg = (function()
        -- Mason installs elixir-ls as a bundle; debugger.sh is the DAP entry point.
        local pkg_dbg = mason_pkg("elixir-ls/debugger.sh")
        if vim.fn.filereadable(pkg_dbg) == 1 then return pkg_dbg end
        -- Some systems ship a standalone elixir-ls-debugger binary.
        local standalone = vim.fn.exepath("elixir-ls-debugger")
        if standalone ~= "" then return standalone end
        -- Do NOT fall back to "elixir-ls" — that is the LSP server, not the debugger.
        return nil
      end)()

      if elixir_dbg then
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
      else
        vim.schedule(function()
          vim.notify(
            "[dap] Elixir DAP debugger not found.\n"
            .. "Run :MasonInstall elixir-ls to install it.",
            vim.log.levels.WARN
          )
        end)
      end

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
      ensure_installed       = { "debugpy", "codelldb", "delve", "js-debug-adapter", "java-debug-adapter", "java-test" },
      automatic_installation = true,
      -- NOTE: handlers key intentionally absent.
      -- handlers={} (empty table) suppresses ALL default adapter setup handlers.
      -- Omitting the key lets mason-nvim-dap run its built-in default handler
      -- for every installed adapter, which is the intended behaviour.
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

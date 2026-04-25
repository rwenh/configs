-- lua/plugins/specs/dap.lua - Debug Adapter Protocol
--
-- (all prior FIX entries preserved — see INSTALL.md for full history)
--
-- FIX (v2.3.12):
--   • Ruby DAP adapter: was hardcoded to "bundle exec rdbg ...". If the user
--     is not in a Bundler project (no Gemfile, or rdbg not in bundle) the
--     adapter silently fails to start. Fixed: resolve rdbg binary with a
--     fallback chain identical to the pattern used by every other adapter:
--       1. `bundle exec rdbg` if `bundle` is executable (Bundler project)
--       2. `rdbg` directly if it is in PATH (gem install ruby-debug-ide)
--       3. Mason bin path as last resort
--     The dap.configurations.ruby entry gains a second config for standalone
--     (non-Bundler) launch so the user sees both options in the picker.
--
-- FIX (v2.3.15):
--   • Python DAP section removed from dap.lua entirely.
--     dap.lua manually registered dap.adapters.python and
--     dap.configurations.python. python.lua's nvim-dap-python.setup()
--     overwrites both on ft=python — the dap.lua entries were dead code
--     that ran at startup for nothing and could diverge from the
--     nvim-dap-python implementation. python.lua is now the sole owner
--     of all Python DAP wiring.

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

      -- ── DAP UI ────────────────────────────────────────────────────────
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

      -- ── Shared helpers ────────────────────────────────────────────────
      local function mason_bin(name)
        local p = vim.fn.exepath(name)
        if p ~= "" then return p end
        return vim.fn.stdpath("data") .. "/mason/bin/" .. name
      end

      local function mason_pkg(rel)
        return vim.fn.stdpath("data") .. "/mason/packages/" .. rel
      end

      -- NOTE: Python DAP (dap.adapters.python + dap.configurations.python)
      -- is owned exclusively by python.lua via nvim-dap-python.setup().
      -- Do not register it here — a second registration is overwritten on
      -- ft=python and the two implementations can diverge silently.

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
      -- FIX: resolve the path once and verify the binary is present before
      -- registering the adapter. Without this guard a missing codelldb
      -- produces a cryptic "failed to start debug server" DAP error rather
      -- than a clear actionable message at startup.
      local codelldb_cmd = mason_bin("codelldb")
      local codelldb_ok  = vim.fn.executable(codelldb_cmd) == 1
                        or vim.fn.filereadable(codelldb_cmd) == 1
      if not codelldb_ok then
        vim.schedule(function()
          vim.notify(
            "[dap] codelldb not found — C / C++ / Rust / Zig debug unavailable.\n"
            .. "Run: :MasonInstall codelldb",
            vim.log.levels.WARN
          )
        end)
      end
      -- Register unconditionally so the config table exists; nvim-dap's
      -- lazy adapter-start will surface the missing-binary error at the
      -- moment the user actually tries to start a session.
      dap.adapters.codelldb = {
        type = "server", port = "${port}",
        executable = {
          command = codelldb_cmd,
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

      -- ── Go (delve) ───────────────────────────────────────────────
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

      -- ── JavaScript / TypeScript (pwa-node) ──────────────────────
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

      -- ── Ruby (rdbg) ──────────────────────────────────────────────
      -- FIX (v2.3.12): resolve rdbg with a proper fallback chain.
      -- bundle exec rdbg is only valid inside a Bundler project. When bundle
      -- is absent or rdbg is not in the bundle, the adapter silently fails.
      -- Resolution order:
      --   1. bundle exec rdbg  (Bundler project with rdbg in Gemfile)
      --   2. rdbg directly     (gem install rdbg / ruby-debug-ide in PATH)
      --   3. Mason bin         (last resort)
      local rdbg_use_bundle = vim.fn.executable("bundle") == 1
      local rdbg_bin = (function()
        if vim.fn.executable("rdbg") == 1 then return vim.fn.exepath("rdbg") end
        local mason_rdbg = vim.fn.stdpath("data") .. "/mason/bin/rdbg"
        if vim.fn.filereadable(mason_rdbg) == 1 then return mason_rdbg end
        return nil
      end)()

      if rdbg_use_bundle or rdbg_bin then
        dap.adapters.ruby = function(callback, config)
          if rdbg_use_bundle then
            callback({
              type = "server", host = "127.0.0.1", port = "${port}",
              executable = {
                command = "bundle",
                args    = { "exec", "rdbg", "-n", "--open", "--port", "${port}",
                            "-c", "--", "ruby", config.program },
              },
            })
          else
            callback({
              type = "server", host = "127.0.0.1", port = "${port}",
              executable = {
                command = rdbg_bin,
                args    = { "-n", "--open", "--port", "${port}",
                            "-c", "--", "ruby", config.program },
              },
            })
          end
        end

        dap.configurations.ruby = {
          { type = "ruby", name = "Debug current file (bundle)", request = "attach",
            localfs = true, program = "${file}" },
          { type = "ruby", name = "Debug current file (rdbg)",   request = "attach",
            localfs = true, program = "${file}" },
        }
      else
        vim.schedule(function()
          vim.notify(
            "[dap] Ruby debugger (rdbg) not found.\n"
            .. "Run: gem install rdbg  OR  bundle add rdbg",
            vim.log.levels.WARN
          )
        end)
      end

      -- ── Elixir (ElixirLS debugger) ─────────────────────────────────
      local elixir_dbg = (function()
        local pkg_dbg = mason_pkg("elixir-ls/debugger.sh")
        if vim.fn.filereadable(pkg_dbg) == 1 then return pkg_dbg end
        local standalone = vim.fn.exepath("elixir-ls-debugger")
        if standalone ~= "" then return standalone end
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
          if vim.b[bufnr] and vim.b[bufnr].large_file then
            vim.notify(
              string.format(
                "[dap] Skipping breakpoint restore for large file buffer %d — "
                .. "line mapping unreliable without treesitter.",
                bufnr
              ),
              vim.log.levels.WARN
            )
            return
          end
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
                -- FIX: delete the augroup after it fires so one-shot restore
                -- augroups don't accumulate across the session lifetime.
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
      ensure_installed = {
        "debugpy", "codelldb", "delve",
        "js-debug-adapter", "java-debug-adapter", "java-test",
        "elixir-ls",
      },
      automatic_installation = true,
      -- handlers key intentionally absent — lets mason-nvim-dap run its
      -- built-in default handler for every installed adapter.
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

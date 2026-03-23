-- lua/plugins/specs/dap.lua - Debug Adapter Protocol
-- F-key and <leader>d* maps are defined in keymaps.lua / here via `keys` table.
-- <leader>; maps live in keymaps.lua — only plugin-lazy-load keys are listed here.

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-neotest/nvim-nio",
    },
    -- Only list keys needed to trigger lazy-loading of this plugin.
    -- Full keymap definitions are in core/keymaps.lua under <leader>; and <F*>.
    keys = {
      { "<F5>" },
      { "<F6>" },
      { "<F7>" },
      { "<F8>" },
      { "<F9>" },
      { "<F10>" },
      { "<F11>" },
      { "<leader>;c" },
      { "<leader>;b" },
    },
    config = function()
      local dap    = require("dap")
      local dapui  = require("dapui")

      -- UI Setup
      dapui.setup({
        layouts = {
          {
            elements = {
              { id = "scopes",      size = 0.25 },
              { id = "breakpoints", size = 0.25 },
              { id = "stacks",      size = 0.25 },
              { id = "watches",     size = 0.25 },
            },
            size     = 40,
            position = "left",
          },
          {
            elements = {
              { id = "repl",    size = 0.5 },
              { id = "console", size = 0.5 },
            },
            size     = 10,
            position = "bottom",
          },
        },
      })

      require("nvim-dap-virtual-text").setup({
        enabled                    = true,
        highlight_changed_variables = true,
        highlight_new_as_changed   = true,
      })

      -- Signs
      vim.fn.sign_define("DapBreakpoint",          { text = "●", texthl = "DapBreakpoint",  linehl = "",             numhl = "" })
      vim.fn.sign_define("DapStopped",             { text = "▶", texthl = "DapStopped",     linehl = "DapStoppedLine", numhl = "" })
      vim.fn.sign_define("DapBreakpointCondition", { text = "◆", texthl = "DapBreakpoint",  linehl = "",             numhl = "" })
      vim.fn.sign_define("DapBreakpointRejected",  { text = "✖", texthl = "DapBreakpoint",  linehl = "",             numhl = "" })
      vim.fn.sign_define("DapLogPoint",            { text = "◉", texthl = "DapLogPoint",    linehl = "",             numhl = "" })

      -- Auto open/close UI
      dap.listeners.after.event_initialized["dapui_config"]  = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"]  = function() dapui.close() end
      dap.listeners.before.event_exited["dapui_config"]      = function() dapui.close() end

      -- ── Python ──────────────────────────────────────────────────────────────
      -- FIX #2: vim.fn.exepath() returns "" (not nil) when binary not found.
      -- "" is truthy in Lua so `or` never falls through — replaced with explicit ~= "" check.
      local function find_python()
        local p3 = vim.fn.exepath("python3")
        if p3 ~= "" then return p3 end
        local p = vim.fn.exepath("python")
        if p ~= "" then return p end
        return "python3"  -- last-resort name so the error message is meaningful
      end

      dap.adapters.python = function(cb, config)
        if config.request == "attach" then
          local port = (config.connect or config).port
          local host = (config.connect or config).host or "127.0.0.1"
          cb({ type = "server", port = assert(port, "`connect.port` required"), host = host,
               options = { source_filetype = "python" } })
        else
          cb({ type = "executable",
               command = find_python(),
               args    = { "-m", "debugpy.adapter" },
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

      -- ── Java (nvim-jdtls overrides at ft attach) ─────────────────────────
      dap.configurations.java = {
        { type = "java", request = "attach", name = "Debug (Attach) - Remote",
          hostName = "127.0.0.1", port = 5005 },
        { type = "java", request = "launch", name = "Debug (Launch) - Current File", mainClass = "${file}" },
      }

      -- ── C / C++ / Rust (codelldb) ─────────────────────────────────────────
      dap.adapters.codelldb = {
        type = "server", port = "${port}",
        executable = {
          command = vim.fn.stdpath("data") .. "/mason/bin/codelldb",
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
        cwd         = "${workspaceFolder}",
        stopOnEntry = false,
      }

      -- FIX #6: Use vim.deepcopy so C and C++ configs are independent table
      -- instances. Sharing the same reference means a runtime mutation to one
      -- (e.g. by a plugin appending env vars) would silently affect the other.
      dap.configurations.c   = { vim.deepcopy(codelldb_launch) }
      dap.configurations.cpp = { vim.deepcopy(codelldb_launch) }
      dap.configurations.rust = {
        vim.tbl_extend("force", codelldb_launch, {
          name    = "Launch file",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/target/debug/", "file")
          end,
        }),
      }

      -- ── Go (delve) ────────────────────────────────────────────────────────
      dap.adapters.delve = {
        type = "server", port = "${port}",
        executable = {
          command = vim.fn.stdpath("data") .. "/mason/bin/dlv",
          args    = { "dap", "-l", "127.0.0.1:${port}" },
        },
      }
      dap.configurations.go = {
        { type = "delve", name = "Debug",              request = "launch", program = "${file}" },
        { type = "delve", name = "Debug test",         request = "launch", mode = "test", program = "${file}" },
        { type = "delve", name = "Debug test (go.mod)", request = "launch", mode = "test", program = "./${relativeFileDirname}" },
      }

      -- ── JavaScript / TypeScript (pwa-node) ───────────────────────────────
      dap.adapters["pwa-node"] = {
        type = "server", host = "localhost", port = "${port}",
        executable = {
          command = "node",
          args    = { vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js", "${port}" },
        },
      }
      dap.configurations.javascript = {
        { type = "pwa-node", request = "launch", name = "Launch file", program = "${file}", cwd = "${workspaceFolder}" },
      }
      dap.configurations.typescript = {
        { type = "pwa-node", request = "launch", name = "Launch file", program = "${file}",
          cwd = "${workspaceFolder}", runtimeExecutable = "ts-node" },
      }

      -- ── Ruby (rdbg) ───────────────────────────────────────────────────────
      dap.adapters.ruby = function(callback, config)
        callback({
          type = "server", host = "127.0.0.1", port = "${port}",
          executable = {
            command = "bundle",
            args    = { "exec", "rdbg", "-n", "--open", "--port", "${port}", "-c", "--", "ruby", config.program },
          },
        })
      end
      dap.configurations.ruby = {
        { type = "ruby", name = "Debug current file", request = "attach", localfs = true, program = "${file}" },
      }

      -- ── Elixir (ElixirLS) ─────────────────────────────────────────────────
      -- FIX #3: exepath() returns "" not nil — or never falls through on miss.
      -- Also replaced hardcoded ~/.local/share/nvim with vim.fn.stdpath("data")
      -- for portability across custom $XDG_DATA_HOME setups.
      local elixir_dbg = vim.fn.exepath("elixir-ls-debugger")
      dap.adapters.mix_task = {
        type    = "executable",
        command = elixir_dbg ~= "" and elixir_dbg
                  or (vim.fn.stdpath("data") .. "/mason/bin/elixir-ls-debugger"),
        args    = {},
      }
      dap.configurations.elixir = {
        { type = "mix_task", name = "mix test", task = "test", taskArgs = { "--trace" },
          request = "launch", startApps = true, projectDir = "${workspaceFolder}",
          requireFiles = { "test/**/test_helper.exs", "test/**/*_test.exs" } },
        { type = "mix_task", name = "mix phx.server", task = "phx.server",
          request = "launch", projectDir = "${workspaceFolder}" },
      }

      -- ── Persistent breakpoints ─────────────────────────────────────────────
      local bp_file = vim.fn.stdpath("data") .. "/dap-breakpoints.json"

      local function save_breakpoints()
        local bps = {}
        for bufnr, buf_bps in pairs(require("dap.breakpoints").get()) do
          local path = vim.api.nvim_buf_get_name(bufnr)
          if path ~= "" and #buf_bps > 0 then
            bps[path] = vim.tbl_map(function(bp)
              -- NOTE: DAP internal field is bp.logMessage (camelCase); we
              -- persist it as log_message (snake_case) to match the set() API.
              return { line = bp.line, condition = bp.condition, log_message = bp.logMessage }
            end, buf_bps)
          end
        end
        -- FIX #5: Skip write when there are no breakpoints to persist.
        if next(bps) == nil then return end
        local f = io.open(bp_file, "w")
        if f then f:write(vim.json.encode(bps)); f:close() end
      end

      local function load_breakpoints()
        local f = io.open(bp_file, "r")
        if not f then return end
        local ok, bps = pcall(vim.json.decode, f:read("*a"))
        f:close()
        if not ok or not bps then return end
        for path, entries in pairs(bps) do
          local bufnr = vim.fn.bufnr(path, true)
          if bufnr ~= -1 then
            for _, bp in ipairs(entries) do
              require("dap.breakpoints").set(
                { condition = bp.condition, log_message = bp.log_message }, bufnr, bp.line)
            end
          end
        end
      end

      vim.api.nvim_create_autocmd("VimLeavePre", { callback = save_breakpoints })
      vim.api.nvim_create_autocmd("VimEnter",    { callback = function() vim.defer_fn(load_breakpoints, 100) end })
    end,
  },

  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason.nvim", "nvim-dap" },
    opts = {
      ensure_installed      = { "python", "codelldb", "delve", "js-debug-adapter" },
      automatic_installation = true,
      handlers              = {},
    },
  },
}

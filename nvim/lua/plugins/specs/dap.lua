-- lua/plugins/specs/dap.lua - Debug Adapter Protocol (updated with Ruby/Elixir)

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-neotest/nvim-nio",
    },
    keys = {
      { "<F5>", function() require("dap").continue() end, desc = "DAP: Continue" },
      { "<F6>", function() require("dap").toggle_breakpoint() end, desc = "DAP: Toggle Breakpoint" },
      { "<F7>", function() require("dap").step_into() end, desc = "DAP: Step Into" },
      { "<F8>", function() require("dap").step_over() end, desc = "DAP: Step Over" },
      { "<F9>", function() require("dap").step_out() end, desc = "DAP: Step Out" },
      { "<F10>", function() require("dap").run_to_cursor() end, desc = "DAP: Run to Cursor" },
      { "<F11>", function() require("dap").terminate() end, desc = "DAP: Terminate" },
      { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint" },
      { "<leader>dB", function() require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: ")) end, desc = "Conditional Breakpoint" },
      { "<leader>dc", function() require("dap").continue() end, desc = "Continue" },
      { "<leader>di", function() require("dap").step_into() end, desc = "Step Into" },
      { "<leader>do", function() require("dap").step_over() end, desc = "Step Over" },
      { "<leader>dO", function() require("dap").step_out() end, desc = "Step Out" },
      { "<leader>dr", function() require("dap").repl.toggle() end, desc = "Toggle REPL" },
      { "<leader>dl", function() require("dap").run_last() end, desc = "Run Last" },
      { "<leader>dt", function() require("dapui").toggle() end, desc = "Toggle UI" },
      { "<leader>dh", function() require("dap.ui.widgets").hover() end, desc = "Hover" },
      { "<leader>dp", function() require("dap.ui.widgets").preview() end, desc = "Preview" },
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")

      -- UI Setup
      dapui.setup({
        layouts = {
          {
            elements = {
              { id = "scopes", size = 0.25 },
              { id = "breakpoints", size = 0.25 },
              { id = "stacks", size = 0.25 },
              { id = "watches", size = 0.25 },
            },
            size = 40,
            position = "left",
          },
          {
            elements = {
              { id = "repl", size = 0.5 },
              { id = "console", size = 0.5 },
            },
            size = 10,
            position = "bottom",
          },
        },
      })

      require("nvim-dap-virtual-text").setup({
        enabled = true,
        highlight_changed_variables = true,
        highlight_new_as_changed = true,
      })

      -- Signs
      vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DapBreakpoint", linehl = "", numhl = "" })
      vim.fn.sign_define("DapStopped", { text = "▶", texthl = "DapStopped", linehl = "DapStoppedLine", numhl = "" })
      vim.fn.sign_define("DapBreakpointCondition", { text = "◆", texthl = "DapBreakpoint", linehl = "", numhl = "" })
      vim.fn.sign_define("DapBreakpointRejected", { text = "✖", texthl = "DapBreakpoint", linehl = "", numhl = "" })
      vim.fn.sign_define("DapLogPoint", { text = "◉", texthl = "DapLogPoint", linehl = "", numhl = "" })

      -- Auto open/close UI
      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
      dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end

      -- Python Adapter
      dap.adapters.python = function(cb, config)
        if config.request == "attach" then
          local port = (config.connect or config).port
          local host = (config.connect or config).host or "127.0.0.1"
          cb({
            type = "server",
            port = assert(port, "`connect.port` is required for a python `attach` configuration"),
            host = host,
            options = { source_filetype = "python" },
          })
        else
          cb({
            type = "executable",
            command = vim.fn.exepath("python3") or vim.fn.exepath("python"),
            args = { "-m", "debugpy.adapter" },
            options = { source_filetype = "python" },
          })
        end
      end

      dap.configurations.python = {
        {
          type = "python",
          request = "launch",
          name = "Launch file",
          program = "${file}",
          pythonPath = function()
            local venv = os.getenv("VIRTUAL_ENV")
            if venv then
              return venv .. "/bin/python"
            end
            return vim.fn.exepath("python3") or vim.fn.exepath("python")
          end,
        },
        {
          type = "python",
          request = "launch",
          name = "Launch file with arguments",
          program = "${file}",
          args = function()
            local args_string = vim.fn.input("Arguments: ")
            return vim.split(args_string, " +")
          end,
          pythonPath = function()
            local venv = os.getenv("VIRTUAL_ENV")
            if venv then
              return venv .. "/bin/python"
            end
            return vim.fn.exepath("python3") or vim.fn.exepath("python")
          end,
        },
        {
          type = "python",
          request = "attach",
          name = "Attach remote",
          connect = function()
            local host = vim.fn.input("Host [127.0.0.1]: ")
            host = host ~= "" and host or "127.0.0.1"
            local port = tonumber(vim.fn.input("Port [5678]: ")) or 5678
            return { host = host, port = port }
          end,
        },
      }

      -- C/C++/Rust Adapter (codelldb)
      dap.adapters.codelldb = {
        type = "server",
        port = "${port}",
        executable = {
          command = vim.fn.stdpath("data") .. "/mason/bin/codelldb",
          args = { "--port", "${port}" },
        },
      }

      dap.configurations.cpp = {
        {
          name = "Launch file",
          type = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
        },
      }

      dap.configurations.c = dap.configurations.cpp
      dap.configurations.rust = {
        {
          name = "Launch file",
          type = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/target/debug/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
        },
      }

      -- Go Adapter (delve)
      dap.adapters.delve = {
        type = "server",
        port = "${port}",
        executable = {
          command = vim.fn.stdpath("data") .. "/mason/bin/dlv",
          args = { "dap", "-l", "127.0.0.1:${port}" },
        },
      }

      dap.configurations.go = {
        {
          type = "delve",
          name = "Debug",
          request = "launch",
          program = "${file}",
        },
        {
          type = "delve",
          name = "Debug test",
          request = "launch",
          mode = "test",
          program = "${file}",
        },
        {
          type = "delve",
          name = "Debug test (go.mod)",
          request = "launch",
          mode = "test",
          program = "./${relativeFileDirname}",
        },
      }

      -- JavaScript/TypeScript Adapter
      dap.adapters["pwa-node"] = {
        type = "server",
        host = "localhost",
        port = "${port}",
        executable = {
          command = "node",
          args = {
            vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js",
            "${port}",
          },
        },
      }

      dap.configurations.javascript = {
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch file",
          program = "${file}",
          cwd = "${workspaceFolder}",
        },
      }

      dap.configurations.typescript = {
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch file",
          program = "${file}",
          cwd = "${workspaceFolder}",
          runtimeExecutable = "ts-node",
        },
      }

      -- Ruby Adapter (rdbg)
      dap.adapters.ruby = function(callback, config)
        callback({
          type = "server",
          host = "127.0.0.1",
          port = "${port}",
          executable = {
            command = "bundle",
            args = { "exec", "rdbg", "-n", "--open", "--port", "${port}", "-c", "--", "ruby", config.program },
          },
        })
      end

      dap.configurations.ruby = {
        {
          type = "ruby",
          name = "Debug current file",
          request = "attach",
          localfs = true,
          program = "${file}",
        },
      }

      -- Elixir Adapter (ElixirLS)
      dap.adapters.mix_task = {
        type = "executable",
        command = vim.fn.exepath("elixir-ls-debugger") or vim.fn.expand("~/.local/share/nvim/mason/bin/elixir-ls-debugger"),
        args = {},
      }

      dap.configurations.elixir = {
        {
          type = "mix_task",
          name = "mix test",
          task = "test",
          taskArgs = { "--trace" },
          request = "launch",
          startApps = true,
          projectDir = "${workspaceFolder}",
          requireFiles = {
            "test/**/test_helper.exs",
            "test/**/*_test.exs",
          },
        },
        {
          type = "mix_task",
          name = "mix phx.server",
          task = "phx.server",
          request = "launch",
          projectDir = "${workspaceFolder}",
        },
      }

      -- Persistent breakpoints
      local breakpoints_file = vim.fn.stdpath("data") .. "/dap-breakpoints.json"

      local function save_breakpoints()
        local breakpoints = {}
        local bps = require("dap.breakpoints").get()
        for bufnr, buf_bps in pairs(bps) do
          local file_path = vim.api.nvim_buf_get_name(bufnr)
          if file_path ~= "" and #buf_bps > 0 then
            breakpoints[file_path] = {}
            for _, bp in ipairs(buf_bps) do
              table.insert(breakpoints[file_path], {
                line = bp.line,
                condition = bp.condition,
                log_message = bp.logMessage,
              })
            end
          end
        end

        local file = io.open(breakpoints_file, "w")
        if file then
          file:write(vim.json.encode(breakpoints))
          file:close()
        end
      end

      local function load_breakpoints()
        local file = io.open(breakpoints_file, "r")
        if not file then return end

        local content = file:read("*a")
        file:close()

        local ok, breakpoints = pcall(vim.json.decode, content)
        if not ok or not breakpoints then return end

        for file_path, bps in pairs(breakpoints) do
          for _, bp in ipairs(bps) do
            local bufnr = vim.fn.bufnr(file_path, true)
            if bufnr ~= -1 then
              require("dap.breakpoints").set({
                condition = bp.condition,
                log_message = bp.log_message,
              }, bufnr, bp.line)
            end
          end
        end
      end

      vim.api.nvim_create_autocmd("VimLeavePre", { callback = save_breakpoints })
      vim.api.nvim_create_autocmd("VimEnter", { 
        callback = function()
          vim.defer_fn(load_breakpoints, 100)
        end
      })
    end,
  },

  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason.nvim", "nvim-dap" },
    opts = {
      ensure_installed = { 
        "python", 
        "codelldb", 
        "delve",
        "js-debug-adapter",
        -- Ruby and Elixir debuggers are installed via gem/mix, not mason
      },
      automatic_installation = true,
      handlers = {},
    },
  },
}
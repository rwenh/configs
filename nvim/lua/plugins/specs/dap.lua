-- lua/plugins/specs/dap.lua - Debug Adapter Protocol

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "rcarriga/nvim-dap-ui",
      "theHamsta/nvim-dap-virtual-text",
      "nvim-neotest/nvim-nio",
    },
    keys = {
      { "<F5>", function() require("dap").continue() end, desc = "Continue" },
      { "<F6>", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
      { "<F7>", function() require("dap").step_into() end, desc = "Step into" },
      { "<F8>", function() require("dap").step_over() end, desc = "Step over" },
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")

      dapui.setup()

      vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DapBreakpoint" })
      vim.fn.sign_define("DapStopped", { text = "▶", texthl = "DapStopped" })

      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
    end,
  },

  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = { "mason.nvim", "nvim-dap" },
    opts = {
      ensure_installed = { "python", "codelldb", "delve" },
      automatic_installation = true,
    },
  },
}
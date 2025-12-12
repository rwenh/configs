-- ~/.config/nvim/lua/config/testing.lua
-- Neotest configuration

local helpers = require("utils.helpers")

helpers.setup_plugin("neotest", function(neotest)
  local adapters = {}
  
  -- Python adapter
  pcall(function()
    table.insert(adapters, require("neotest-python")({
      dap = { justMyCode = false },
      args = { "-q" },
      runner = "pytest",
    }))
  end)
  
  -- Rust adapter
  pcall(function()
    table.insert(adapters, require("neotest-rust")({
      args = { "--nocapture" },
    }))
  end)
  
  -- Jest adapter
  pcall(function()
    table.insert(adapters, require("neotest-jest")({
      jestCommand = "npm test --",
    }))
  end)
  
  -- Plenary adapter
  pcall(function()
    table.insert(adapters, require("neotest-plenary")())
  end)
  
  neotest.setup({
    adapters = adapters,
    output = { enabled = true, open_on_run = false },
    output_panel = { enabled = true, open = "vsplit" },
    quickfix = { open = false },
    floating = { border = "rounded", max_width = 0.7, max_height = 0.7 },
    diagnostic = { enabled = true, severity = vim.diagnostic.severity.ERROR },
    summary = { animated = false, expand_errors = true },
  })
end)

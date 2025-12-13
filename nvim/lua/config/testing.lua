-- ~/.config/nvim/lua/config/testing.lua
-- Neotest configuration

local helpers = require("utils.helpers")

helpers.setup_plugin("neotest", function(neotest)
  local adapters = {}
  
  -- Python adapter
  pcall(function()
    table.insert(adapters, require("neotest-python")({
      dap = { justMyCode = false },
      args = { "-q", "--no-header" },
      runner = "pytest",
    }))
  end)
  
  -- Rust adapter
  pcall(function()
    table.insert(adapters, require("neotest-rust")({
      args = { "--nocapture" },
      dap_adapter = "codelldb",
    }))
  end)
  
  -- Jest adapter (JS/TS)
  pcall(function()
    table.insert(adapters, require("neotest-jest")({
      jestCommand = "npm test --",
      jestConfigFile = "jest.config.js",
      env = { CI = true },
      cwd = function()
        return vim.fn.getcwd()
      end,
    }))
  end)
  
  -- Go adapter
  pcall(function()
    table.insert(adapters, require("neotest-go")({
      experimental = {
        test_table = true,
      },
      args = { "-count=1", "-timeout=60s" }
    }))
  end)
  
  -- Plenary adapter (Neovim plugin tests)
  pcall(function()
    table.insert(adapters, require("neotest-plenary")())
  end)
  
  neotest.setup({
    adapters = adapters,
    output = { 
      enabled = true, 
      open_on_run = false 
    },
    output_panel = { 
      enabled = true, 
      open = "botright vsplit | vertical resize 80" 
    },
    quickfix = { 
      enabled = true,
      open = false 
    },
    floating = { 
      border = "rounded", 
      max_width = 0.8, 
      max_height = 0.8 
    },
    diagnostic = { 
      enabled = true, 
      severity = vim.diagnostic.severity.ERROR 
    },
    summary = { 
      animated = true, 
      expand_errors = true,
      follow = true,
      mappings = {
        attach = "a",
        clear_marked = "M",
        clear_target = "T",
        debug = "d",
        debug_marked = "D",
        expand = { "<CR>", "<2-LeftMouse>" },
        expand_all = "e",
        jumpto = "i",
        mark = "m",
        next_failed = "J",
        output = "o",
        prev_failed = "K",
        run = "r",
        run_marked = "R",
        short = "O",
        stop = "u",
        target = "t",
      },
    },
    status = {
      enabled = true,
      virtual_text = true,
      signs = true,
    },
    icons = {
      passed = "",
      running = "",
      failed = "",
      skipped = "",
      unknown = "",
    },
  })
end)

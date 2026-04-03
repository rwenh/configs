-- lua/plugins/specs/test.lua - Testing

return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-python",
      "rouge8/neotest-rust",
      "nvim-neotest/neotest-go",
      "olimorris/neotest-rspec",
      "jfpedroza/neotest-elixir",
      "marilari88/neotest-vitest",
      "haydenmeade/neotest-jest",
      "rcasia/neotest-java",
    },
    keys = {
      { "<leader>'n", function() pcall(function() require("neotest").run.run() end) end, desc = "Test Nearest" },
      { "<leader>'f", function() pcall(function() require("neotest").run.run(vim.fn.expand("%")) end) end, desc = "Test File" },
      { "<leader>'a", function() pcall(function() require("neotest").run.run(vim.uv.cwd()) end) end, desc = "Test All" },
      { "<leader>'u", function() pcall(function() require("neotest").summary.toggle() end) end, desc = "Test Summary" },
      { "<leader>'o", function() pcall(function() require("neotest").output.open({ enter = true }) end) end, desc = "Test Output" },
      { "<leader>'p", function() pcall(function() require("neotest").output_panel.toggle() end) end, desc = "Test Panel" },
      { "<leader>'d", function() pcall(function() require("neotest").run.run({ strategy = "dap" }) end) end, desc = "Test Debug Nearest" },
    },
    opts = function()
      -- RECALIBRATION: Safe adapter loading
      local adapters = {}

      local adapter_configs = {
        { "neotest-python", function() return require("neotest-python")({ runner = "pytest" }) end },
        { "neotest-rust", function() return require("neotest-rust") end },
        { "neotest-go", function() return require("neotest-go") end },
        { "neotest-rspec", function()
          return require("neotest-rspec")({ rspec_cmd = function() return { "bundle", "exec", "rspec" } end })
        end },
        { "neotest-elixir", function() return require("neotest-elixir") end },
        { "neotest-vitest", function() return require("neotest-vitest") end },
        { "neotest-jest", function() return require("neotest-jest")({ jestCommand = "npm test --" }) end },
        { "neotest-java", function() return require("neotest-java")({ ignore_wrapper = false }) end },
      }

      for _, config in ipairs(adapter_configs) do
        local name, loader = config[1], config[2]
        local ok, adapter = pcall(loader)
        if ok and adapter then
          table.insert(adapters, adapter)
        else
          vim.notify("Failed to load " .. name, vim.log.levels.WARN)
        end
      end

      return {
        adapters = adapters,
        status   = { virtual_text = true },
        output   = { open_on_run = true },
        quickfix = { open = false },
      }
    end,
  },

  {
    "andythigpen/nvim-coverage",
    dependencies = "nvim-lua/plenary.nvim",
    cmd  = "Coverage",
    opts = {},
    keys = {
      { "<leader>tcv", "<cmd>Coverage<cr>", desc = "Coverage Load" },
      { "<leader>tcs", "<cmd>CoverageSummary<cr>", desc = "Coverage Summary" },
      { "<leader>tct", "<cmd>CoverageToggle<cr>", desc = "Coverage Toggle" },
    },
  },
}

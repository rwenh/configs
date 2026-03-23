-- lua/plugins/specs/test.lua - Testing

return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      -- Core adapters
      "nvim-neotest/neotest-python",
      "rouge8/neotest-rust",
      "nvim-neotest/neotest-go",
      -- Extended adapters
      "olimorris/neotest-rspec",
      "jfpedroza/neotest-elixir",
      "marilari88/neotest-vitest",
      "haydenmeade/neotest-jest",
      "rcasia/neotest-java",       -- covers Kotlin (Gradle/Maven JUnit)
    },
    keys = {
      { "<leader>'n", function() require("neotest").run.run() end,                   desc = "Test Nearest" },
      { "<leader>'f", function() require("neotest").run.run(vim.fn.expand("%")) end,  desc = "Test File" },
      { "<leader>'a", function() require("neotest").run.run(vim.uv.cwd()) end,       desc = "Test All" },
      { "<leader>'u", function() require("neotest").summary.toggle() end,            desc = "Test Summary" },
      { "<leader>'o", function() require("neotest").output.open({ enter = true }) end, desc = "Test Output" },
      { "<leader>'p", function() require("neotest").output_panel.toggle() end,       desc = "Test Panel" },
      -- FIX #10: Renamed <leader>td → <leader>'d to resolve collision with
      -- lsp.lua's <leader>td (Type Definition). Neotest keys now consistently
      -- live under the <leader>' run/test namespace from keymaps.lua.
      { "<leader>'d", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Test Debug Nearest" },
    },
    opts = function()
      return {
        adapters = {
          require("neotest-python")({ runner = "pytest" }),
          require("neotest-rust"),
          require("neotest-go"),
          require("neotest-rspec")({
            -- FIX #11: vim.tbl_flatten deprecated in Neovim 0.11 and was a
            -- no-op here anyway (no nested tables). Direct table construction
            -- is identical and forward-compatible.
            rspec_cmd = function()
              return { "bundle", "exec", "rspec" }
            end,
          }),
          require("neotest-elixir"),
          require("neotest-vitest"),
          require("neotest-jest")({ jestCommand = "npm test --" }),
          require("neotest-java")({ ignore_wrapper = false }),
        },
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
      { "<leader>tcv", "<cmd>Coverage<cr>",       desc = "Coverage Load" },
      { "<leader>tcs", "<cmd>CoverageSummary<cr>", desc = "Coverage Summary" },
      { "<leader>tct", "<cmd>CoverageToggle<cr>",  desc = "Coverage Toggle" },
    },
  },
}

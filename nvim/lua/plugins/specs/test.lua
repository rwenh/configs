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
    },
    keys = {
      { "<leader>tn", function() require("neotest").run. run() end, desc = "Run nearest" },
      { "<leader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run file" },
      { "<leader>ta", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Run all" },
      { "<leader>ts", function() require("neotest").summary. toggle() end, desc = "Summary" },
    },
    opts = function()
      return {
        adapters = {
          require("neotest-python")({ runner = "pytest" }),
          require("neotest-rust"),
          require("neotest-go"),
        },
      }
    end,
  },

  {
    "andythigpen/nvim-coverage",
    dependencies = "nvim-lua/plenary.nvim",
    cmd = "Coverage",
    opts = {},
  },
}
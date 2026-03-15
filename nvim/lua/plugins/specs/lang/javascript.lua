-- lua/plugins/specs/lang/javascript.lua

return {
  -- DAP: vscode-js adapter
  {
    "mxsdev/nvim-dap-vscode-js",
    ft           = { "javascript", "javascriptreact" },
    dependencies = { "mfussenegger/nvim-dap" },
    opts         = { adapters = { "pwa-node", "pwa-chrome" } },
  },

  -- Neotest adapter for JS (vitest/jest)
  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = { "marilari88/neotest-vitest", "haydenmeade/neotest-jest" },
    opts = function(_, opts)
      opts.adapters = opts.adapters or {}
      table.insert(opts.adapters, require("neotest-vitest"))
      table.insert(opts.adapters, require("neotest-jest")({
        jestCommand = "npm test --",
      }))
    end,
  },

  -- Package.json dependency management
  {
    "vuki656/package-info.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    ft           = "json",
    event        = "BufRead package.json",
    opts         = { colors = { up_to_date = "#3C4048", outdated = "#d19a66" } },
    keys = {
      { "<leader>jps", function() require("package-info").show() end,          desc = "Show package versions" },
      { "<leader>jpu", function() require("package-info").update() end,        desc = "Update package" },
      { "<leader>jpd", function() require("package-info").delete() end,        desc = "Delete package" },
      { "<leader>jpi", function() require("package-info").install() end,       desc = "Install package" },
      { "<leader>jpc", function() require("package-info").change_version() end, desc = "Change version" },
    },
  },

  -- Lint: eslint_d
  {
    "mfussenegger/nvim-lint",
    optional = true,
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = vim.tbl_extend("force", lint.linters_by_ft or {}, {
        javascript      = { "eslint_d" },
        javascriptreact = { "eslint_d" },
      })
    end,
  },

  -- Conform: prettier
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        javascript      = { "prettier" },
        javascriptreact = { "prettier" },
      },
    },
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "javascript", "jsdoc" })
      end
    end,
  },
}

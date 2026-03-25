-- lua/plugins/specs/lang/javascript.lua

return {
  -- NOTE (Fix #1): nvim-dap-vscode-js removed — dap.lua already registers
  -- the pwa-node adapter directly via the js-debug-adapter Mason package.
  -- Having both caused pwa-node to be registered twice, second silently
  -- overwriting the first.

  -- NOTE (Fix #3): neotest-vitest and neotest-jest adapter registrations
  -- removed — test.lua already registers both centrally. Inserting them here
  -- too caused each adapter to run twice per test file, producing duplicate
  -- results. The adapter plugins themselves are still installed as dependencies
  -- of neotest in test.lua.

  -- Package.json dependency management
  {
    "vuki656/package-info.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    -- FIX #4: Removed ft = "json" — loaded package-info for ALL json files.
    -- Only package.json needs it; the BufRead event is precise and sufficient.
    event = "BufRead package.json",
    opts  = { colors = { up_to_date = "#3C4048", outdated = "#d19a66" } },
    keys = {
      { "<leader>jps", function() require("package-info").show() end,           desc = "Show package versions" },
      { "<leader>jpu", function() require("package-info").update() end,         desc = "Update package" },
      { "<leader>jpd", function() require("package-info").delete() end,         desc = "Delete package" },
      { "<leader>jpi", function() require("package-info").install() end,        desc = "Install package" },
      { "<leader>jpc", function() require("package-info").change_version() end, desc = "Change version" },
    },
  },

  -- Lint: eslint_d
  {
    "mfussenegger/nvim-lint",
    optional = true,
    -- FIX #2: optional=true config doesn't run — moved to init.
    -- Targeted assignment avoids overwriting lsp.lua's linter table.
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.js", "*.jsx" },
        once     = true,
        callback = function()
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.javascript      = { "eslint_d" }
          lint.linters_by_ft.javascriptreact = { "eslint_d" }
        end,
      })
    end,
  },

  -- Conform: prettier for JSX (JS already registered in lsp.lua)
  {
    "stevearc/conform.nvim",
    optional = true,
    -- FIX #5: Removed javascript entry — lsp.lua already registers it.
    -- Kept javascriptreact which lsp.lua doesn't cover.
    opts = {
      formatters_by_ft = {
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

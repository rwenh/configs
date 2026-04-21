-- lua/plugins/specs/lang/javascript.lua
--
-- OPT (v2.3.13):
--   • nvim-lint spec: BufReadPost + once=true autocmd wrapper removed.
--     Direct init() assignment is sufficient (see css.lua note).

return {
  -- ── Package.json dependency management ───────────────────────────────
  {
    "vuki656/package-info.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    event = "BufRead package.json",
    opts  = { colors = { up_to_date = "#3C4048", outdated = "#d19a66" } },
    config = function(_, opts)
      pcall(function() require("package-info").setup(opts) end)
    end,
    keys = {
      { "<leader>jps", function() pcall(function() require("package-info").show()           end) end, desc = "Show package versions" },
      { "<leader>jpu", function() pcall(function() require("package-info").update()         end) end, desc = "Update package" },
      { "<leader>jpd", function() pcall(function() require("package-info").delete()         end) end, desc = "Delete package" },
      { "<leader>jpi", function() pcall(function() require("package-info").install()        end) end, desc = "Install package" },
      { "<leader>jpc", function() pcall(function() require("package-info").change_version() end) end, desc = "Change version" },
    },
  },

  -- ── nvim-lint ─────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.javascript      = { "eslint_d" }
      lint.linters_by_ft.javascriptreact = { "eslint_d" }
    end,
  },

  -- ── Conform ───────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.javascriptreact = { "prettier" }
    end,
  },

  -- ── Treesitter ────────────────────────────────────────────────────────
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

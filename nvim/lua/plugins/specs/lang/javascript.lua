-- lua/plugins/specs/lang/javascript.lua — JavaScript development
--
-- LSP:    typescript-tools (typescript.lua) covers JS too
-- Format: prettier via lsp.lua conform (javascript + javascriptreact)
-- Lint:   eslint_d via lsp.lua nvim-lint (guarded)
-- DAP:    pwa-node via dap.lua
-- Test:   neotest-jest/vitest via test.lua; runner.lua
--
-- NOTE: package-info.nvim makes npm registry network requests on show().
--       It requires internet access and npm registry availability.
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── Package.json dependency management ────────────────────────────────────

  {
    "vuki656/package-info.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    event = "BufRead package.json",
    cond  = function()
      -- Don't load for dependency package.json files inside node_modules.
      return not vim.fn.expand("%:p"):find("node_modules", 1, true)
    end,
    opts  = { colors = { up_to_date = "#3C4048", outdated = "#d19a66" } },
    config = function(_, opts)
      pcall(function() require("package-info").setup(opts) end)
    end,
    keys = (function()
      local function pkg_action(fn_name, desc)
        return {
          "<leader>jp" .. fn_name:sub(1, 1):lower(),
          function()
            pcall(function() require("package-info")[fn_name]() end)
          end,
          desc = desc,
        }
      end
      return {
        { "<leader>jps", function()
            pcall(function() require("package-info").show() end)
          end, desc = "Show package versions (requires network)" },
        { "<leader>jpu", function()
            pcall(function() require("package-info").update() end)
          end, desc = "Update package" },
        { "<leader>jpd", function()
            pcall(function() require("package-info").delete() end)
          end, desc = "Delete package" },
        { "<leader>jpi", function()
            pcall(function() require("package-info").install() end)
          end, desc = "Install package" },
        { "<leader>jpc", function()
            pcall(function() require("package-info").change_version() end)
          end, desc = "Change version" },
      }
      _ = pkg_action  -- suppress unused; kept as reference
    end)(),
  },

  -- ── Conform: javascriptreact ───────────────────────────────────────────────

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.javascriptreact = { "prettier" }
    end,
  },

  -- ── nvim-lint: eslint_d ────────────────────────────────────────────────────

  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      if vim.fn.executable("eslint_d") ~= 1 then return end
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      -- Only register filetypes not already covered by lsp.lua's merge_linters.
      -- lsp.lua guards eslint_d and registers all four ft variants when present.
    end,
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "javascript", "jsdoc" })
      end
    end,
  },

  -- NOTE: typescript-tools.nvim handles both JS and TS LSP (typescript.lua).
  -- eslint_d linting is owned by lsp.lua (guarded with executable check).
  -- DAP pwa-node adapter is in dap.lua.
  -- neotest-jest and neotest-vitest adapters are in test.lua.
}

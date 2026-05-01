-- lua/plugins/specs/lang/ruby.lua — Ruby language support
--
-- LSP:    solargraph via lsp.lua
-- Format: rubocop via lsp.lua conform
-- Lint:   rubocop via lsp.lua nvim-lint
-- DAP:    rdbg via dap.lua (deferred FileType)
-- Test:   vim-test (this file) + neotest-rspec (test.lua) + runner.lua
--
-- Test mechanism guide:
--   <leader>rbn/f/s  → vim-test (buffer-local, toggleterm strategy)
--   <leader>'n       → neotest-rspec (nearest test with rich output)
--   <leader>'t       → runner.lua (bundle exec rspec from project root)
--   Use neotest for interactive TDD; vim-test for quick file/suite runs;
--   runner for full CI-style suite execution.
--

return {
  -- ── vim-test ───────────────────────────────────────────────────────────────

  {
    "vim-test/vim-test",
    ft   = "ruby",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "ruby",
        once     = true,
        group    = vim.api.nvim_create_augroup("VimTestRubyStrategy", { clear = true }),
        callback = function()
          vim.g["test#strategy"]                    = "toggleterm"
          vim.g["test#toggleterm#reuse_terminal"]   = 1
        end,
      })
    end,
    keys = (function()
      local function rkey(lhs, cmd, desc)
        return { lhs, "<cmd>" .. cmd .. "<cr>", desc = desc, ft = "ruby" }
      end
      return {
        rkey("<leader>rbn", "TestNearest", "Ruby Test Nearest"),
        rkey("<leader>rbf", "TestFile",    "Ruby Test File"   ),
        rkey("<leader>rbs", "TestSuite",   "Ruby Test Suite"  ),
        rkey("<leader>rbl", "TestLast",    "Ruby Test Last"   ),
        rkey("<leader>rbv", "TestVisit",   "Ruby Test Visit"  ),
      }
    end)(),
  },

  -- ── vim-rails ──────────────────────────────────────────────────────────────

  {
    "tpope/vim-rails",
    ft   = "ruby",
    cond = function()
      return vim.fn.filereadable(
        vim.fn.findfile("config/application.rb", ".;")
      ) == 1
    end,
  },

  -- ── endwise ────────────────────────────────────────────────────────────────

  {
    "RRethy/nvim-treesitter-endwise",
    event        = "InsertEnter",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },

  -- ── Treesitter (cross-ref) ─────────────────────────────────────────────────

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "ruby" })
      end
    end,
  },

  -- NOTE: rubocop formatter/linter and solargraph LSP are in lsp.lua.
  -- rdbg DAP adapter is in dap.lua.
  -- neotest-rspec adapter is in test.lua.
}

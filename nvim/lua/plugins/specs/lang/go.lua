-- lua/plugins/specs/lang/go.lua — Go language support
--
-- LSP:    gopls via lsp.lua
-- Format: goimports + gofumpt via lsp.lua conform
-- Lint:   staticcheck via go.nvim (built-in)
-- DAP:    delve via dap.lua (deferred FileType)
-- Test:   neotest-go via test.lua; go.nvim :GoTest; runner.lua run_tests()
--
-- Test mechanism guide:
--   <leader>'t   → runner.lua  (go test ./... from project root)
--   <leader>'n   → neotest     (nearest test with rich output panel)
--   <leader>got  → go.nvim     (GoTest — integrates with go.nvim's env setup)
--   Use neotest for interactive development; runner for CI-style full runs.
--

local GO_FT = { "go", "gomod" }

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft    = GO_FT,
    build = false,
    config = function()
      local ok = pcall(function()
        require("go").setup({
          lsp_cfg = false,
        })
      end)
      if not ok then
        vim.notify("go.nvim setup failed", vim.log.levels.WARN)
      end
    end,
    keys = {
      { "<leader>got", "<cmd>GoTest<cr>",     desc = "Go Test",          ft = GO_FT },
      { "<leader>gof", "<cmd>GoTestFunc<cr>", desc = "Go Test Function", ft = GO_FT },
      { "<leader>goc", "<cmd>GoCoverage<cr>", desc = "Go Coverage",      ft = GO_FT },
      { "<leader>gor", "<cmd>GoRun<cr>",      desc = "Go Run",           ft = GO_FT },
      { "<leader>gob", "<cmd>GoBuild<cr>",    desc = "Go Build",         ft = GO_FT },
      { "<leader>goi", "<cmd>GoImpl<cr>",     desc = "Go Impl",          ft = GO_FT },
      { "<leader>goa", "<cmd>GoAddTag<cr>",   desc = "Go Add Tag",       ft = GO_FT },
      { "<leader>gom", "<cmd>GoMod<cr>",      desc = "Go Mod",           ft = GO_FT },
    },
  },

  -- This optional spec is a no-op marker that makes go.lua the canonical
  -- reference for Go formatting — lsp.lua remains the actual owner.
  -- Uncomment and populate if Go formatting is ever moved here:
  -- {
  --   "stevearc/conform.nvim",
  --   optional = true,
  --   opts = function(_, opts)
  --     opts.formatters_by_ft = opts.formatters_by_ft or {}
  --     opts.formatters_by_ft.go = { "goimports", "gofumpt" }
  --   end,
  -- },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "go", "gomod", "gowork", "gosum" })
      end
    end,
  },
}

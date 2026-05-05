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

local shared = require("plugins.specs.lang.shared")
local GO_FT = { "go", "gomod" }

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft    = GO_FT,
    build = false,
    config = function()
      local ok = pcall(function()
        require("go").setup({ lsp_cfg = false })
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

  -- Go formatting (goimports + gofumpt) is owned by lsp.lua conform config.
  shared.treesitter({ "go", "gomod", "gowork", "gosum" }),
}

-- lua/plugins/specs/lang/go.lua - Go development

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft    = { "go", "gomod" },
    -- FIX #5: Added build step — go.nvim requires this for treesitter
    -- go parser and some UI features to initialise correctly.
    build = ":lua require('go.install').update_all_sync()",
    config = function()
      require("go").setup({
        -- FIX #3: Disabled lsp_cfg — go.nvim would launch its own gopls
        -- instance alongside the one already configured in lsp.lua, attaching
        -- two gopls clients per Go buffer (duplicate diagnostics/completions).
        -- lsp.lua owns gopls via vim.lsp.config/enable (Neovim 0.11+ API).
        lsp_cfg = false,
        -- FIX #4: gopls settings removed — lsp.lua already configures
        -- analyses/staticcheck/gofumpt for gopls. Redundant with lsp_cfg=false.
      })
    end,
    keys = {
      { "<leader>got", "<cmd>GoTest<cr>",     desc = "Go Test",          ft = "go" },
      { "<leader>gof", "<cmd>GoTestFunc<cr>", desc = "Go Test Function", ft = "go" },
      { "<leader>goc", "<cmd>GoCoverage<cr>", desc = "Go Coverage",      ft = "go" },
      { "<leader>gor", "<cmd>GoRun<cr>",      desc = "Go Run",           ft = "go" },
      { "<leader>gob", "<cmd>GoBuild<cr>",    desc = "Go Build",         ft = "go" },
      { "<leader>goi", "<cmd>GoImpl<cr>",     desc = "Go Impl",          ft = "go" },
      { "<leader>goa", "<cmd>GoAddTag<cr>",   desc = "Go Add Tag",       ft = "go" },
      { "<leader>gom", "<cmd>GoMod<cr>",      desc = "Go Mod",           ft = "go" },
    },
  },
}

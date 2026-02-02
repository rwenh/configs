-- lua/plugins/specs/lang/go.lua - Go development (SAFE KEYMAPS)

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft = { "go", "gomod" },
    config = function()
      require("go").setup({
        lsp_cfg = {
          settings = {
            gopls = {
              analyses = { unusedparams = true },
              staticcheck = true,
              gofumpt = true,
            },
          },
        },
      })
    end,
    keys = {
      { "<leader>got", "<cmd>GoTest<cr>", desc = "Go Test", ft = "go" },
      { "<leader>gof", "<cmd>GoTestFunc<cr>", desc = "Go Test Function", ft = "go" },
      { "<leader>goc", "<cmd>GoCoverage<cr>", desc = "Go Coverage", ft = "go" },
      { "<leader>gor", "<cmd>GoRun<cr>", desc = "Go Run", ft = "go" },
      { "<leader>gob", "<cmd>GoBuild<cr>", desc = "Go Build", ft = "go" },
      { "<leader>goi", "<cmd>GoImpl<cr>", desc = "Go Impl", ft = "go" },
      { "<leader>goa", "<cmd>GoAddTag<cr>", desc = "Go Add Tag", ft = "go" },
      { "<leader>gom", "<cmd>GoMod<cr>", desc = "Go Mod", ft = "go" },
    },
  },
}

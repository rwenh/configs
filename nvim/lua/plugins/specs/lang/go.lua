-- lua/plugins/specs/lang/go.lua - Go development

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
      { "<leader>gt", "<cmd>GoTest<cr>", desc = "Go Test", ft = "go" },
      { "<leader>gf", "<cmd>GoTestFunc<cr>", desc = "Go Test Function", ft = "go" },
      { "<leader>gc", "<cmd>GoCoverage<cr>", desc = "Go Coverage", ft = "go" },
    },
  },
}
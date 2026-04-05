-- lua/plugins/specs/lang/go.lua - Go development

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft    = { "go", "gomod" },
    build = ":lua require('go.install').update_all_sync()",
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
      { "<leader>got", "<cmd>GoTest<cr>",     desc = "Go Test",          ft = "go" },
      { "<leader>gof", "<cmd>GoTestFunc<cr>", desc = "Go Test Function", ft = "go" },
      { "<leader>goc", "<cmd>GoCoverage<cr>", desc = "Go Coverage",      ft = "go" },
      { "<leader>gor", "<cmd>GoRun<cr>",      desc = "Go Run",           ft = "go" },
      { "<leader>gob", "<cmd>GoBuild<cr>",    desc = "Go Build",         ft = "go" },
      { "<leader>goi", "<cmd>GoImpl<cr>",      desc = "Go Impl",          ft = "go" },
      { "<leader>goa", "<cmd>GoAddTag<cr>",   desc = "Go Add Tag",       ft = "go" },
      { "<leader>gom", "<cmd>GoMod<cr>",      desc = "Go Mod",           ft = "go" },
    },
  },
}

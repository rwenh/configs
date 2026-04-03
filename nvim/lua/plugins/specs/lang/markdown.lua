-- nvim/lua/plugins/specs/lang/markdown.lua

return {
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown" },
    build = function(plugin)
      if vim.fn.executable("npm") == 1 then
        vim.fn.system("cd " .. plugin.dir .. "/app && npm install --legacy-peer-deps 2>/dev/null")
      end
    end,
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 1
      vim.g.mkdp_refresh_slow = 0
      vim.g.mkdp_combine_preview = 1
      vim.g.mkdp_preview_options = {
        mkit = {},
        katex = {},
        uml = {},
        maid = {},
        disable_sync_scroll = 0,
        sync_scroll_type = "middle",
      }
    end,
    keys = {
      { "<leader>mp", "<cmd>MarkdownPreviewToggle<CR>", desc = "Markdown Preview Toggle" },
    },
  },

  {
    "preservim/vim-markdown",
    ft = { "markdown" },
    init = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_frontmatter = 1
      vim.g.vim_markdown_toml_frontmatter = 1
    end,
  },

  {
    "dhruvasagar/vim-table-mode",
    ft = { "markdown" },
    cmd = "TableModeToggle",
    keys = {
      { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Table Mode Toggle" },
    },
  },
}

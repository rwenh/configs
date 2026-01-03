-- lua/plugins/specs/lang/markdown.lua - Markdown writing

return {
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview" },
    ft = "markdown",
    build = function() vim.fn["mkdp#util#install"]() end,
  },

  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft = "markdown",
    opts = {},
  },
}

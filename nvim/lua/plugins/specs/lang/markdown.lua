-- lua/plugins/specs/lang/markdown.lua
--
-- FIX (v2.2.3):
--   • render-markdown: render_modes = { "n", "c" } — "c" (command-line mode)
--     is not a valid render mode and produced a startup warning. Valid modes
--     are "n" (normal), "i" (insert), "v"/"V"/"" (visual variants).
--     Removed "c"; kept only "n" as the sane default.

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
      vim.g.mkdp_filetypes       = { "markdown" }
      vim.g.mkdp_auto_start      = 0
      vim.g.mkdp_auto_close      = 1
      vim.g.mkdp_refresh_slow    = 0
      vim.g.mkdp_combine_preview = 1
      vim.g.mkdp_preview_options = {
        mkit = {}, katex = {}, uml = {}, maid = {},
        disable_sync_scroll = 0,
        sync_scroll_type    = "middle",
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
      vim.g.vim_markdown_frontmatter      = 1
      vim.g.vim_markdown_toml_frontmatter = 1
    end,
  },

  {
    "dhruvasagar/vim-table-mode",
    ft  = { "markdown" },
    cmd = "TableModeToggle",
    keys = {
      { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Table Mode Toggle" },
    },
  },

  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft           = { "markdown" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      enabled      = true,
      -- FIX: "c" removed — not a valid render mode; caused startup warning.
      render_modes = { "n" },
      anti_conceal = { enabled = true },
      heading = {
        enabled = true,
        signs   = { "󰫎 ", "󰫎 ", "󰫎 ", "󰫎 ", "󰫎 ", "󰫎 " },
        width   = "block",
      },
      code = {
        enabled   = true,
        sign      = true,
        style     = "full",
        border    = "thin",
        width     = "block",
        left_pad  = 1,
        right_pad = 1,
        min_width = 20,
        above     = "▄",
        below     = "▀",
        highlight = "RenderMarkdownCode",
      },
      bullet = {
        enabled = true,
        icons   = { "●", "○", "◆", "◇" },
      },
      checkbox = {
        enabled   = true,
        unchecked = { icon = "󰄱 " },
        checked   = { icon = "󰱒 " },
      },
      quote  = { enabled = true },
      table  = { enabled = true },
      link   = { enabled = true },
    },
    config = function(_, opts)
      local ok = pcall(function() require("render-markdown").setup(opts) end)
      if not ok then
        vim.notify("render-markdown.nvim setup failed", vim.log.levels.WARN)
      end
    end,
  },
}

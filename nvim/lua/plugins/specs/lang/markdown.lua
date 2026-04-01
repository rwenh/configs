-- lua/plugins/specs/lang/markdown.lua - Markdown writing

return {
  -- Browser preview
  {
    "iamcco/markdown-preview.nvim",
    -- FIX #3: Removed ft = "markdown" — with both cmd and ft triggers the
    -- plugin loaded on every markdown file open, not just when commands are
    -- used. cmd-only is sufficient and avoids the eager load.
    cmd   = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    -- FIX #2: Removed require("lazy").load(...) from build — build runs in
    -- the plugin's own context already; forcing a load here caused issues when
    -- build ran before Lazy had finished its setup.
    build = "cd app && npm install",
    init  = function() vim.g.mkdp_filetypes = { "markdown" } end,
    version = false,
  },

  -- Rich render inside Neovim
  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft           = "markdown",
    -- FIX #1: Added explicit treesitter dependency — render-markdown requires
    -- treesitter to parse and render markdown elements correctly.
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {
      heading = { enabled = true, sign = false },
      code    = { enabled = true, sign = false },
      bullet  = { enabled = true },
    },
  },

  -- Table formatting
  {
    "dhruvasagar/vim-table-mode",
    ft   = "markdown",
    init = function()
      vim.g.table_mode_corner = "|"
    end,
    keys = {
      { "<leader>mdt", "<cmd>TableModeToggle<cr>",   desc = "Markdown Table Mode" },
      { "<leader>mdf", "<cmd>TableModeRealign<cr>",  desc = "Markdown Table Realign" },
    },
  },

  -- Paste image from clipboard
  {
    "HakonHarnes/img-clip.nvim",
    ft   = "markdown",
    opts = {
      default = {
        dir_path   = "assets",
        file_name  = "%Y-%m-%d-%H-%M-%S",
        use_absolute_path = false,
      },
    },
    keys = {
      { "<leader>mdp", "<cmd>PasteImage<cr>", desc = "Markdown Paste Image" },
    },
  },

  -- Conform: prettier for markdown
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        markdown = { "prettier" },
      },
    },
  },

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "markdown", "markdown_inline" })
      end
    end,
  },
}

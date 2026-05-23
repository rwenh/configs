-- lua/plugins/specs/lang/markdown.lua — Markdown development
--
-- Architecture:
--   render-markdown.nvim — in-editor rendered Markdown (concealment, icons)
--   markdown-preview.nvim — browser preview (live HTML render)
--   vim-markdown         — syntax, frontmatter, folding (disabled; treesitter owns it)
--   vim-table-mode       — table editing helper
-- All four cover different concerns; none are redundant.

local icons = require("core.util.icons")

return {
  -- ── Markdown Preview (browser) ─────────────────────────────────────────────
  {
    "iamcco/markdown-preview.nvim",
    cmd   = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft    = { "markdown" },

    build = function(plugin)
      if vim.fn.executable("npm") ~= 1 then
        vim.notify(
          "[markdown-preview] npm not found — browser preview will not work.\n"
          .. "Install npm (nodejs) and run :Lazy build markdown-preview.nvim.",
          vim.log.levels.WARN
        )
        return
      end

      local out = vim.fn.system(
        "cd " .. vim.fn.shellescape(plugin.dir) .. "/app"
        .. " && npm install --legacy-peer-deps"
      )

      if vim.v.shell_error ~= 0 then
        vim.notify(
          "[markdown-preview] npm install failed — browser preview will not work.\n"
          .. "Run :Lazy build markdown-preview.nvim to retry.\n"
          .. "npm output:\n" .. vim.trim(out),
          vim.log.levels.WARN
        )
      end
    end,

    init = function()
      vim.g.mkdp_filetypes  = { "markdown" }
      vim.g.mkdp_auto_start = 0   -- don't auto-open on buffer enter
      vim.g.mkdp_auto_close = 1   -- auto-close when buffer changes
      vim.g.mkdp_preview_options = {
        sync_scroll_type = "middle",
      }
    end,
    keys = {
      { "<leader>mp", "<cmd>MarkdownPreviewToggle<CR>", desc = "Markdown Preview Toggle" },
    },
  },

  -- ── vim-markdown ───────────────────────────────────────────────────────────
  {
    "preservim/vim-markdown",
    ft   = { "markdown" },
    init = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_frontmatter      = 1
      vim.g.vim_markdown_toml_frontmatter = 1
    end,
  },

  -- ── Table mode ─────────────────────────────────────────────────────────────
  {
    "dhruvasagar/vim-table-mode",
    ft   = { "markdown" },
    cmd  = "TableModeToggle",
    keys = {
      { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Table Mode Toggle" },
    },
  },

  -- ── render-markdown.nvim ───────────────────────────────────────────────────

  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft           = { "markdown" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      enabled      = true,
      render_modes = { "n" },
      anti_conceal = {
        enabled = true,
        ignore  = { code_background = true, sign = true },
      },
      heading = {
        enabled = true,
        signs   = icons.headings,
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
    },
    config = function(_, opts)
      local ok = pcall(function() require("render-markdown").setup(opts) end)
      if not ok then
        vim.notify("render-markdown.nvim setup failed", vim.log.levels.WARN)
      end
    end,
  },
}

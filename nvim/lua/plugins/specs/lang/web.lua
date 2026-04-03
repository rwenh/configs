-- nvim/lua/plugins/specs/lang/web.lua - Web development

return {
  {
    "luckasRanarison/tailwind-tools.nvim",
    lazy = true,
    event = "VeryLazy",
    ft = { "html", "javascript", "javascriptreact", "typescript", "typescriptreact", "css", "vue", "svelte", "jsx", "tsx" },
    opts = {
      document_color = {
        enabled = true,
        kind = "background",
        inline_strategy = "none",
      },
      cmp = {
        enabled = true,
        min_priority = 2,
      },
      conceal = {
        enabled = false,
      },
      custom_filetypes = {},
    },
    config = function(_, opts)
      pcall(function() require("tailwind-tools").setup(opts) end)
    end,
  },

  {
    "windwp/nvim-ts-autotag",
    ft = { "html", "javascript", "javascriptreact", "typescriptreact", "jsx", "tsx", "vue", "svelte" },
    config = function()
      pcall(function() require("nvim-ts-autotag").setup() end)
    end,
  },

  {
    "mattn/emmet-vim",
    ft = { "html", "css", "javascript", "javascriptreact", "typescriptreact", "vue", "svelte" },
    init = function()
      vim.g.user_emmet_leader_key = "<C-y>"
    end,
  },
  -- NOTE: colorizer is owned by advanced.lua (NvChad/nvim-colorizer.lua).
  -- That plugin is a maintained fork of norcalli/nvim-colorizer.lua;
  -- speccing both causes a startup conflict — removed here.
}

-- lua/plugins/specs/advanced.lua - Advanced features

return {
  -- Escape without timeout delay (solves the timeoutlen lag on 'j' when using jk/kj mappings)
  -- Native inoremap jk <Esc> still causes Neovim to wait `timeoutlen` ms after every 'j'.
  -- better-escape.nvim intercepts at the Lua level and fires immediately — zero perceived lag.
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts = {
      -- FIX #2: timeout (200ms) and options.lua timeoutlen (500ms) are independent
      -- but related — if timeoutlen is ever lowered below this value, jk/kj
      -- recognition becomes slower than a native mapping would have been.
      -- Change both together if you tune timeoutlen.
      timeout          = 200,
      default_mappings = false,
      mappings = {
        i = { j = { k = "<Esc>" }, k = { j = "<Esc>" } },
        -- FIX #3: Removed visual mode (v) mappings. In visual mode j/k are
        -- navigation keys — mapping jk to escape means typing j then k to
        -- move down one line would escape instead. Almost certainly unintended.
      },
    },
  },

  -- Symbol navigation (breadcrumb context in winbar)
  -- FIX #1: Added explicit lazy = true. Without a trigger, this spec would
  -- load at an undefined point. Attachment to LSP clients happens in lsp.lua
  -- via LspAttach (navic.attach(client, bufnr)) — this spec only installs the
  -- plugin and sets default options.
  {
    "SmiteshP/nvim-navic",
    lazy         = true,
    dependencies = "neovim/nvim-lspconfig",
    opts = {
      separator   = " > ",
      highlight   = true,
      depth_limit = 5,
    },
  },

  -- Colorizer
  {
    "NvChad/nvim-colorizer.lua",
    -- FIX #4: Changed BufReadPre → BufReadPost. BufReadPre fires before
    -- buffer content is loaded — colorizer was scanning an empty buffer on
    -- first attach and never re-scanning. BufReadPost fires after content
    -- is present so the initial scan actually finds colour strings.
    event = "BufReadPost",
    opts = {
      filetypes = { "*", "!lazy" },
      user_default_options = {
        RGB      = true,
        RRGGBB   = true,
        RRGGBBAA = true,
        css      = true,
        tailwind = true,
        mode     = "background",
      },
    },
  },

  -- Rainbow brackets
  {
    "HiPhish/rainbow-delimiters.nvim",
    event = "VeryLazy",
  },

  -- Motion repeat (enables '.' for plugin maps)
  {
    "tpope/vim-repeat",
    event = "VeryLazy",
  },

  -- File type icons (loaded on-demand by other plugins via require)
  {
    "nvim-tree/nvim-web-devicons",
    lazy = true,
  },
}

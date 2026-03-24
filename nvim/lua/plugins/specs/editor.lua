-- lua/plugins/specs/editor.lua - Core editor plugins

return {
  -- File explorer
  {
    "nvim-tree/nvim-tree.lua",
    lazy = false,
    dependencies = "nvim-tree/nvim-web-devicons",
    opts = {
      hijack_netrw = false,
      view      = { width = 35 },
      renderer  = { group_empty = true, highlight_git = true, indent_markers = { enable = true } },
      filters   = { custom = { "node_modules", ".cache", "__pycache__" }, dotfiles = false },
      git       = { enable = true, ignore = false },
      actions   = { open_file = { quit_on_open = false } },
    },
  },

  -- Fuzzy finder
  {
    "nvim-telescope/telescope.nvim",
    cmd          = "Telescope",
    dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    opts = {
      defaults = {
        prompt_prefix    = "  ",
        selection_caret  = " ",
        sorting_strategy = "ascending",
        layout_strategy  = "horizontal",
        layout_config    = {
          horizontal = { prompt_position = "top", preview_width = 0.55 },
          width = 0.87, height = 0.80,
        },
      },
    },
    config = function(_, opts)
      require("telescope").setup(opts)
      require("telescope").load_extension("fzf")
    end,
  },

  -- Comments
  -- FIX #13: nvim-ts-context-commentstring v0.8+ removed the
  -- integrations.comment_nvim module. The correct v0.8+ pattern is:
  -- call ts_context_commentstring.setup() with enable_autocmd=false,
  -- then use its integrations.comment_nvim pre_hook which still exists
  -- but is now initialized differently. If you are on <v0.8, remove the
  -- setup() call and keep only the pre_hook line.
  {
    "numToStr/Comment.nvim",
    dependencies = "JoosepAlviste/nvim-ts-context-commentstring",
    keys = {
      { "gcc", mode = "n" },
      { "gc",  mode = { "n", "v" } },
    },
    config = function()
      require("ts_context_commentstring").setup({ enable_autocmd = false })
      require("Comment").setup({
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
      })
    end,
  },

  -- Surround
  { "kylechui/nvim-surround", event = "VeryLazy", opts = {} },

  -- Autopairs
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    -- FIX #9: map_cr = false prevents autopairs from inserting a closing bracket
    -- on <CR> — blink.cmp's auto_brackets (completion.lua) already handles this
    -- on completion accept. Without this, <CR> to accept a completion would
    -- produce a doubled closing bracket.
    opts  = { check_ts = true, map_cr = false },
    config = function(_, opts)
      require("nvim-autopairs").setup(opts)
    end,
  },

  -- Highlight word under cursor
  {
    "RRethy/vim-illuminate",
    event  = { "BufReadPost", "BufNewFile" },
    opts   = { delay = 200 },
    config = function(_, opts) require("illuminate").configure(opts) end,
  },

  -- Better motions
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts  = { modes = { search = { enabled = false }, char = { enabled = false } } },
    keys  = {
      { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
    },
  },

  -- Todo comments
  {
    "folke/todo-comments.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts  = {},
    keys  = {
      { "]t", function() require("todo-comments").jump_next() end, desc = "Next todo" },
      { "[t", function() require("todo-comments").jump_prev() end, desc = "Previous todo" },
    },
  },

  -- Mini modules
  { "echasnovski/mini.ai",   event = "VeryLazy", opts = { n_lines = 500 } },
  { "echasnovski/mini.move", event = "VeryLazy", opts = {} },

  -- Undo tree
  {
    "mbbill/undotree",
    cmd  = "UndotreeToggle",
    keys = { { "<leader>uu", "<cmd>UndotreeToggle<cr>", desc = "Undotree" } },
  },

  -- Better quickfix
  { "kevinhwang91/nvim-bqf", ft = "qf", opts = {} },

  -- Session management
  {
    "rmagatti/auto-session",
    lazy = false,
    opts = {
      suppressed_dirs = { "~/", "~/Downloads", "/" },
      auto_save       = true,
      auto_restore    = true,
    },
    -- FIX #8: Removed redundant init block that set vim.o.sessionoptions.
    -- options.lua already owns this setting with the same value.
    -- Having two places set the same option creates a false appearance of
    -- conflict. options.lua is the single source of truth for all vim options.
  },

  -- Zen mode
  {
    "folke/zen-mode.nvim",
    cmd  = "ZenMode",
    opts = {},
    keys = { { "<leader>uz", "<cmd>ZenMode<cr>", desc = "Zen mode" } },
  },

  -- Color picker
  {
    "uga-rosa/ccc.nvim",
    cmd  = { "CccPick", "CccConvert" },
    opts = { highlighter = { auto_enable = true, lsp = true } },
  },

  -- Maximize window
  {
    "szw/vim-maximizer",
    keys = { { "<leader>sm", "<cmd>MaximizerToggle<cr>", desc = "Maximize" } },
  },

  -- Code outline
  {
    "stevearc/aerial.nvim",
    cmd  = "AerialToggle",
    opts = {
      backends = { "lsp", "treesitter", "markdown" },
      layout   = { default_direction = "prefer_right" },
    },
    keys = { { "<leader>,o", "<cmd>AerialToggle<cr>", desc = "Code outline" } },
  },

  -- Search and replace
  {
    "nvim-pack/nvim-spectre",
    cmd  = "Spectre",
    opts = {},
    keys = { { "<leader>/s", function() require("spectre").open() end, desc = "Search & replace" } },
  },

  -- Better fold
  {
    "kevinhwang91/nvim-ufo",
    dependencies = "kevinhwang91/promise-async",
    event        = "BufReadPost",
    opts = {
      provider_selector = function() return { "treesitter", "indent" } end,
    },
    config = function(_, opts)
      require("ufo").setup(opts)
      vim.keymap.set("n", "zR", require("ufo").openAllFolds)
      vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
    end,
  },

  -- Scrollbar
  { "petertriho/nvim-scrollbar", event = "BufReadPost", opts = {} },

  -- Code actions preview
  -- FIX #11: Added keys trigger — without any event/cmd/keys this spec had no
  -- lazy-load trigger and would never load (defaults.lazy = true). Wired to
  -- the same <leader>,a binding used in keymaps.lua so it intercepts code
  -- actions and shows the preview UI. Remove if not using this plugin.
  {
    "aznhe21/actions-preview.nvim",
    keys = {
      { "<leader>,a", function() require("actions-preview").code_actions() end,
        mode = { "n", "v" }, desc = "Code action (preview)" },
    },
    opts = {},
  },

  -- Refactoring
  {
    "ThePrimeagen/refactoring.nvim",
    cmd          = "Refactor",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
    opts         = {},
  },

  -- Harpoon for file navigation
  {
    "ThePrimeagen/harpoon",
    branch       = "harpoon2",
    dependencies = "nvim-lua/plenary.nvim",
    keys = {
      { "<leader>ha", function() require("harpoon"):list():add() end,                                                      desc = "Harpoon add" },
      { "<leader>hm", function() require("harpoon").ui:toggle_quick_menu(require("harpoon"):list()) end,                   desc = "Harpoon menu" },
      { "<leader>h1", function() require("harpoon"):list():select(1) end,                                                  desc = "Harpoon 1" },
      { "<leader>h2", function() require("harpoon"):list():select(2) end,                                                  desc = "Harpoon 2" },
      { "<leader>h3", function() require("harpoon"):list():select(3) end,                                                  desc = "Harpoon 3" },
      { "<leader>h4", function() require("harpoon"):list():select(4) end,                                                  desc = "Harpoon 4" },
      -- FIX #12: <C-number> not transmitted by most terminals — changed to
      -- <M-number> (Alt+number) to match the same fix applied in keymaps.lua.
      { "<M-1>", function() require("harpoon"):list():select(1) end },
      { "<M-2>", function() require("harpoon"):list():select(2) end },
      { "<M-3>", function() require("harpoon"):list():select(3) end },
      { "<M-4>", function() require("harpoon"):list():select(4) end },
    },
  },
}

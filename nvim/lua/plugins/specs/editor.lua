-- lua/plugins/specs/editor.lua
--
-- FIX (v2.2.4):
--   • telescope-fzf-native: build command is a cmake pipeline. When cmake is
--     absent the build step fails silently (lazy swallows build errors), fzf
--     native never compiles, and telescope falls back to the slow Lua sorter
--     with no warning. Added cond guard: if cmake is unavailable, fall back to
--     the pure-make build (which only needs a C compiler and make, available
--     on almost all systems). If neither is present the plugin loads without
--     native sorting — better than a silent half-broken state.
--   • persistence.nvim: options evaluated lazily via config function so
--     sessionoptions reflects the value set in options.lua (unchanged).

return {

  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    event = "VeryLazy",
    opts = {
      settings = {
        save_on_toggle  = true,
        save_on_change  = true,
        enter_on_select = false,
        key = function() return vim.loop.cwd() end,
      },
    },
    config = function(_, opts)
      local ok = pcall(function() require("harpoon").setup(opts) end)
      if not ok then
        vim.notify("harpoon setup failed", vim.log.levels.WARN)
      end
    end,
  },

  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzf-native.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      local ok = pcall(function()
        require("telescope").setup({
          defaults = {
            layout_strategy = "horizontal",
            layout_config = {
              height          = 0.9,
              preview_cutoff  = 120,
              prompt_position = "bottom",
            },
            sorting_strategy = "ascending",
            file_ignore_patterns = {
              "node_modules", ".git", "dist", "build", ".venv", "__pycache__",
            },
          },
          extensions = {
            fzf = {
              fuzzy                   = true,
              override_generic_sorter = true,
              override_file_sorter    = true,
              case_mode               = "smart_case",
            },
          },
        })
        -- fzf extension load is a no-op if native build didn't compile
        pcall(function() require("telescope").load_extension("fzf") end)
      end)
      if not ok then
        vim.notify("telescope setup failed", vim.log.levels.WARN)
      end
    end,
  },

  {
    "nvim-telescope/telescope-fzf-native.nvim",
    lazy = true,
    -- FIX: cmake build fails silently when cmake is absent, leaving telescope
    -- without native sorting and no user-visible warning. Strategy:
    --   1. If cmake exists → use the full cmake pipeline (best performance).
    --   2. If only make/cc exist → use the simple `make` build.
    --   3. If neither → skip build entirely; telescope still works, just slower.
    build = (function()
      if vim.fn.executable("cmake") == 1 then
        return "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
      elseif vim.fn.executable("make") == 1 then
        return "make"
      end
      -- No suitable build tool — return empty string, lazy skips the step.
      return ""
    end)(),
    cond = function()
      -- Load the plugin even if the native build failed — telescope degrades
      -- gracefully to the Lua sorter. We just notify the user once.
      if vim.fn.executable("cmake") == 0 and vim.fn.executable("make") == 0 then
        vim.schedule(function()
          vim.notify(
            "[telescope-fzf-native] cmake and make both unavailable — using slow Lua sorter.\n"
            .. "Install cmake or make for native sorting performance.",
            vim.log.levels.WARN
          )
        end)
      end
      return true
    end,
  },

  {
    "nvim-pack/nvim-spectre",
    cmd          = "Spectre",
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      color_devicons = true,
      open_cmd       = "noswapfile vnew",
      find_engine = {
        rg = {
          cmd  = "rg",
          args = { "--color=never", "--no-heading", "--with-filename",
                   "--line-number", "--column" },
        },
      },
      replace_engine = { sed = { cmd = "sed", args = nil } },
    },
  },

  {
    "folke/todo-comments.nvim",
    cmd          = { "TodoTelescope" },
    event        = "VeryLazy",
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      signs     = false,
      highlight = {
        multiline         = true,
        multiline_pattern = "^.",
        multiline_context = 10,
        before            = "",
        keyword           = "wide",
        after             = "fg",
        pattern           = [[.*<(KEYWORDS)\s*:]],
      },
      colors = {
        error   = { "DiagnosticError", "ErrorMsg",   "#DC2626" },
        warning = { "DiagnosticWarn",  "WarningMsg", "#FBBF24" },
        info    = { "DiagnosticInfo",                "#2563EB" },
        hint    = { "DiagnosticHint",                "#10B981" },
        default = { "Identifier",                    "#7C3AED" },
      },
      keywords = {
        FIX  = { icon = " ", color = "error",   alt = { "FIXME", "BUG", "FIXIT", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning" },
        WARN = { icon = " ", color = "warning", alt = { "WARNING", "XXX" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint",    alt = { "INFO" } },
        TEST = { icon = "⚙ ", color = "test",   alt = { "TESTING", "PASSED", "FAILED" } },
      },
    },
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    cmd    = "Neotree",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    keys = {
      { "<leader>ee", "<cmd>Neotree reveal<cr>",  desc = "Toggle explorer" },
      { "<leader>ef", "<cmd>Neotree focus<cr>",   desc = "Focus explorer" },
      { "<leader>ec", "<cmd>Neotree close<cr>",   desc = "Close explorer" },
      { "<leader>er", "<cmd>Neotree refresh<cr>", desc = "Refresh explorer" },
    },
    opts = {
      close_if_last_window    = false,
      popup_border_style      = "rounded",
      enable_git_status       = true,
      enable_diagnostics      = true,
      default_component_configs = {
        container = { enable_character_fade = false },
        indent = {
          indent_size = 2, padding = 1, with_markers = true,
          indent_marker = "│", last_indent_marker = "└",
          highlight = "NeoTreeIndentMarker",
          with_expanders = nil,
          expander_collapsed = "", expander_expanded = "",
          expander_highlight = "NeoTreeExpander",
        },
        icon = {
          folder_closed = "", folder_open = "", folder_empty = "󰜌",
          default = "*", highlight = "NeoTreeFileIcon",
        },
        modified = { symbol = "[+]", highlight = "NeoTreeModified" },
        name     = { trailing_slash = false, use_git_status_colors = true,
                     highlight = "NeoTreeFileName" },
        git_status = {
          symbols = {
            added = "", deleted = "", modified = "", renamed = "",
            untracked = "", ignored = "", unstaged = "󰄱",
            staged = "", conflict = "",
          },
        },
      },
      window = {
        position = "left", width = 30,
        mapping_options = { noremap = true, nowait = true },
        mappings = {
          ["<space>"]       = { "toggle_node", nowait = false },
          ["<2-LeftMouse>"] = "open",
          ["<cr>"]          = "open",
          ["<esc>"]         = "revert_preview",
          ["P"]             = { "toggle_preview", config = { use_float = true } },
          ["l"]             = "focus_preview",
          ["S"]             = "open_split",
          ["s"]             = "open_vsplit",
          ["t"]             = "open_tabnew",
          ["w"]             = "open_with_window_picker",
          ["C"]             = "close_node",
          ["z"]             = "close_all_nodes",
          ["Z"]             = "expand_all_nodes",
          ["a"]             = { "add",           config = { show_path = "none" } },
          ["A"]             = { "add_directory", config = { show_path = "none" } },
          ["d"]             = "delete",
          ["r"]             = "rename",
          ["c"]             = "copy",
          ["x"]             = "cut",
          ["p"]             = "paste",
          ["y"]             = "copy_to_clipboard",
          ["Y"]             = "cut_to_clipboard",
          ["?"]             = "show_help",
          ["<"]             = "prev_source",
          [">"]             = "next_source",
          ["i"]             = "show_file_details",
        },
      },
      filesystem = {
        filtered_items = {
          visible        = false,
          hide_dotfiles  = true,
          hide_gitignored = true,
          hide_hidden    = true,
          hide_by_name   = { ".DS_Store", "thumbs.db", "node_modules" },
          hide_by_pattern = { "*.swp", "*.swo", "*~" },
          always_show    = { ".env", ".gitignore" },
          never_show     = { ".DS_Store", "thumbs.db" },
        },
        follow_current_file    = { enabled = true, leave_dirs_open = false },
        hijack_netrw_behavior  = "open_current",
        use_libuv              = true,
        window = {
          mappings = {
            ["<bs>"]  = "navigate_up",
            ["."]     = "set_root",
            ["H"]     = "toggle_hidden",
            ["/"]     = "fuzzy_finder",
            ["D"]     = "fuzzy_finder_directory",
            ["#"]     = "fuzzy_sorter",
            ["f"]     = "filter_on_submit",
            ["<c-x>"] = "clear_filter",
            ["[g"]    = "prev_git_modified",
            ["]g"]    = "next_git_modified",
          },
        },
      },
      buffers = {
        follow_current_file = { enabled = true, leave_dirs_open = false },
        group_empty_dirs    = true,
        show_unloaded       = true,
      },
      git_status = {
        window = {
          position = "float",
          mappings = {
            ["A"]  = "git_add_all",
            ["gu"] = "git_unstage_file",
            ["ga"] = "git_add_file",
            ["gr"] = "git_revert_file",
            ["gc"] = "git_commit",
            ["gp"] = "git_push",
            ["gg"] = "git_commit_and_push",
          },
        },
      },
    },
  },

  -- FIX: flash.nvim spec added. keymaps.lua maps `s` to flash.jump() but the
  -- plugin was never specced — require("flash") always failed in pcall.
  -- event=VeryLazy ensures it loads before any `s` keypress in normal use.
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts  = {
      modes = {
        search = { enabled = false }, -- don't hijack /
        char   = { enabled = false }, -- don't hijack f/t/F/T
      },
    },
    config = function(_, opts)
      pcall(function() require("flash").setup(opts) end)
    end,
  },

  { "echasnovski/mini.move",   version = false, event = "VeryLazy",
    config = function() require("mini.move").setup() end },

  { "echasnovski/mini.pairs",  version = false, event = "VeryLazy",
    config = function() require("mini.pairs").setup() end },

  {
    "nvimdev/template.nvim",
    cmd = "Template",
    config = function()
      local ok, template = pcall(require, "template")
      if not ok then
        vim.notify("template.nvim setup failed", vim.log.levels.WARN)
        return
      end
      pcall(function()
        template.setup({
          temp_dir     = vim.fn.stdpath("config") .. "/templates",
          auto_name    = false,
          author_name  = "Your Name",
          author_email = "your.email@example.com",
          web_devicons = true,
        })
      end)
    end,
  },

  {
    "jay-babu/mason-null-ls.nvim",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = { "mason.nvim", "nvim-lint" },
    opts = {
      ensure_installed       = nil,
      automatic_installation = true,
      handlers               = {},
    },
  },

  {
    "folke/persistence.nvim",
    event = "BufReadPre",
    opts  = {},
    config = function()
      require("persistence").setup({
        dir     = vim.fn.stdpath("state") .. "/sessions/",
        options = vim.opt.sessionoptions:get(),
        pre_save = nil,
      })
    end,
    keys = {
      { "<leader>qs", function() require("persistence").load() end,
        desc = "Restore session (cwd)" },
      { "<leader>ql", function() require("persistence").load({ last = true }) end,
        desc = "Restore last session" },
      { "<leader>qd", function() require("persistence").stop() end,
        desc = "Don't save session on exit" },
    },
  },
}

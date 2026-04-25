-- nvim/lua/plugins/specs/treesitter.lua
--
-- (all prior FIX entries preserved — see INSTALL.md for full history)
--
-- FIX (v2.3.12):
--   • "scala", "swift", "r", "perl", "php", "dart" removed from
--     ensure_installed. None of these have corresponding lang specs, LSP
--     configs, formatters, linters, or keymaps anywhere in the project.
--     They added compile time and disk usage on every fresh install for
--     zero functional benefit. auto_install=true means they will still be
--     fetched on-demand if the user opens a file of that type.
--   • highlight.disable threshold comment updated to reflect v2.3.12 change.

return {
  {
    "nvim-treesitter/nvim-treesitter",
    build    = ":TSUpdate",
    lazy     = false,
    priority = 100,
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    config = function(_, opts)
      -- Suppress the broken vim highlights query on Neovim 0.11.x
      pcall(vim.treesitter.query.set, "vim", "highlights", "")

      local ok, ts_configs = pcall(require, "nvim-treesitter.configs")
      if not ok then
        vim.notify(
          "nvim-treesitter not ready yet. Run :TSUpdate to install parsers.",
          vim.log.levels.WARN
        )
        return
      end
      ts_configs.setup(opts)
    end,
    opts = {
      ensure_installed = {
        -- Core systems / shell
        "bash", "c", "cmake", "cpp", "diff",
        "git_rebase", "gitcommit",
        -- Web
        "css", "html", "http",
        "javascript", "jsdoc", "json", "json5", "jsonc",
        "typescript", "tsx",
        "vue", "svelte",
        -- Lua
        "lua", "luadoc", "luap",
        -- Prose
        "markdown", "markdown_inline",
        -- Scripted languages with full lang specs
        "python", "ruby", "elixir", "heex", "eex",
        -- Systems languages with full lang specs
        "rust", "go", "zig", "fortran", "vhdl",
        -- JVM languages with full lang specs
        "java", "kotlin",
        -- Data / config
        "sql", "toml", "yaml", "query", "regex",
        -- Misc with lang specs
        "julia",
        -- Neovim docs
        "vimdoc",
        -- comment: required by todo-comments.nvim (multiline) and noice
        "comment",
        -- auto_install=true handles these on-demand if the user opens such files.
      },

      auto_install   = true,
      sync_install   = false,
      ignore_install = { "vim" },

      highlight = {
        enable  = true,
        disable = function(lang, buf)
          if vim.b[buf] and vim.b[buf].large_file then return true end
          -- FIX: vim.uv.fs_stat is synchronous here; on network-mounted paths
          -- it blocks the main loop. Cache the result in a buffer variable so
          -- the stat fires at most once per buffer lifetime, not on every
          -- treesitter attach/re-check call.
          if vim.b[buf] ~= nil and vim.b[buf]._ts_size_checked then
            return (vim.b[buf]._ts_large == true) or lang == "vim"
          end
          local max_filesize = 500 * 1024
          local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
          local is_large = ok and stats and stats.size > max_filesize
          pcall(function()
            vim.b[buf]._ts_size_checked = true
            vim.b[buf]._ts_large        = is_large and true or false
          end)
          if is_large then return true end
          return lang == "vim"
        end,
        additional_vim_regex_highlighting = false,
      },

      indent = {
        enable  = true,
        disable = { "python", "yaml", "vue", "html" },
      },

      incremental_selection = {
        enable  = true,
        keymaps = {
          init_selection    = "<C-space>",
          node_incremental  = "<C-space>",
          scope_incremental = false,
          node_decremental  = "<bs>",
        },
      },

      rainbow = { enable = false },

      textobjects = {
        select = {
          enable    = true,
          lookahead = true,
          keymaps   = {
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
            ["al"] = "@loop.outer",
            ["il"] = "@loop.inner",
            ["aa"] = "@parameter.outer",
            ["ia"] = "@parameter.inner",
            ["ai"] = "@conditional.outer",
            ["ii"] = "@conditional.inner",
          },
        },
        move = {
          enable    = true,
          set_jumps = true,
          goto_next_start = {
            ["]f"] = "@function.outer",
            ["]c"] = "@class.outer",
            ["]a"] = "@parameter.outer",
          },
          goto_next_end = {
            ["]F"] = "@function.outer",
            ["]C"] = "@class.outer",
            ["]A"] = "@parameter.outer",
          },
          goto_previous_start = {
            ["[f"] = "@function.outer",
            ["[c"] = "@class.outer",
            ["[a"] = "@parameter.outer",
          },
          goto_previous_end = {
            ["[F"] = "@function.outer",
            ["[C"] = "@class.outer",
            ["[A"] = "@parameter.outer",
          },
        },
        swap = {
          enable        = true,
          swap_next     = { ["<leader>sa"] = "@parameter.inner" },
          swap_previous = { ["<leader>sA"] = "@parameter.inner" },
        },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    lazy = true,
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    event        = "BufReadPost",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      enable              = true,
      max_lines           = 3,
      min_window_height   = 0,
      multiline_threshold = 20,
      trim_scope          = "outer",
      mode                = "topline",
      zindex              = 20,
    },
    config = function(_, opts)
      local ok, ctx = pcall(require, "treesitter-context")
      if ok then
        ctx.setup(opts)
        vim.keymap.set("n", "<leader>uc", ctx.go_to_context,
          { silent = true, desc = "Go to context start" })
      end
    end,
  },
}

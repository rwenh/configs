-- nvim/lua/plugins/specs/treesitter.lua
--
-- FIX (v2.2.4):
--   • tag="v0.9.3" hard-pin removed. The pin conflicted with auto_install=true:
--     any parser requiring a grammar ABI newer than 0.9.3 compiled against the
--     wrong header and silently produced empty highlights. Removed pin so lazy
--     installs the latest stable treesitter release; ABI negotiation is handled
--     by nvim-treesitter itself.
--   • fold = {enable=true} inside nvim-treesitter opts has NO effect. Treesitter
--     folding is enabled via vim.opt (foldmethod/foldexpr), not through the
--     configs.setup() table. The dead key was removed; fold setup moved to
--     options.lua (foldmethod="expr", foldexpr via autocmd on BufReadPost).
--   • vim parser still explicitly ignored (broken query on 0.11.x).
--
-- FIX (v2.3.7):
--   • "comment" removed from ignore_install. The comment treesitter parser is
--     used by todo-comments.nvim for multiline TODO detection (editor.lua sets
--     multiline=true) and by noice.nvim's long_message_to_split preset. Ignoring
--     it silently disabled multiline todo highlighting with no error or warning.
--     Only "vim" remains in ignore_install (broken highlight query on 0.11.x).

return {
  {
    "nvim-treesitter/nvim-treesitter",
    -- FIX: tag pin removed — hard-pinning to v0.9.3 caused ABI mismatches
    -- with parsers that require a newer grammar ABI. lazy.nvim will now track
    -- the latest stable release and auto_install will compile against the
    -- correct header version.
    build    = ":TSUpdate",
    lazy     = false,
    priority = 100,
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    config = function(_, opts)
      -- Override the broken vim highlights query that references a
      -- non-existent (parameter) node on Neovim 0.11.x
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
        "bash", "c", "cmake", "cpp", "css", "diff",
        "git_rebase", "gitcommit", "go", "html", "http",
        "javascript", "jsdoc", "json", "json5", "jsonc",
        "latex", "lua", "luadoc", "luap",
        "markdown", "markdown_inline",
        "python", "query", "regex", "rust",
        "sql", "toml", "typescript",
        "vimdoc", "yaml",
        "fortran", "julia", "zig", "ruby",
        "elixir", "heex", "eex",
        "kotlin", "vhdl", "java", "scala",
        "swift", "r", "perl", "php",
        "dart", "vue", "svelte",
        -- comment parser: needed by todo-comments.nvim (multiline) and noice
        "comment",
      },

      auto_install   = true,
      sync_install   = false,
      -- FIX (v2.3.7): "comment" removed — it is required by todo-comments.nvim
      -- and noice.nvim. Only "vim" remains: its highlights query references a
      -- non-existent node type on Neovim 0.11.x and must be suppressed.
      ignore_install = { "vim" },

      highlight = {
        enable  = true,
        disable = function(lang, buf)
          local max_filesize = 100 * 1024
          local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
          if ok and stats and stats.size > max_filesize then return true end
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
      -- FIX: fold key removed. nvim-treesitter.configs.setup() does not
      -- honour a "fold" key — treesitter folding is enabled exclusively via:
      --   vim.opt.foldmethod = "expr"
      --   vim.opt.foldexpr   = "nvim_treesitter#foldexpr()"
      -- Both are set in options.lua. Leaving the dead key here caused
      -- confusion about why folding "wasn't working" despite being "enabled".

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
          enable = true,
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

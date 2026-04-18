-- nvim/lua/plugins/specs/treesitter.lua
--
-- FIX (v2.2.4):
--   • tag pin removed; fold dead key removed.
--
-- FIX (v2.3.1):
--   • foldexpr updated (in options.lua).
--
-- FIX (v2.3.8):
--   • "comment" removed from ignore_install — required by todo-comments.nvim
--     and noice.nvim. Only "vim" remains in ignore_install.
--
-- FIX (v2.3.9b):
--   • "latex" removed from ensure_installed. No latex LSP, lang spec, or
--     formatter exists anywhere in the project. The parser was installed on
--     every setup but never used, adding unnecessary compile time and disk
--     usage. rainbow-delimiters.nvim references "latex" strategy/query keys
--     but those are no-ops when the parser is absent — no functional impact.
--
-- FIX (v2.3.10):
--   • highlight.disable threshold raised from 100 KB to 500 KB to match the
--     large_file threshold in autocmds.lua and nvim-ufo's provider_selector
--     guard in advanced.lua. A file between 100–500 KB previously had
--     treesitter highlights silently disabled here while ufo still attempted
--     to attach the treesitter fold provider — an inconsistent state.
--   • highlight.disable now also checks vim.b.large_file so the autocmd-set
--     flag is honoured on re-enter without a redundant fs_stat call.

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
      -- Override the broken vim highlights query on Neovim 0.11.x
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
        -- FIX (v2.3.9b): "latex" removed — no corresponding LSP, lang spec,
        -- formatter, or linter exists in this config. Parser was dead weight.
        "lua", "luadoc", "luap",
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
      -- Only "vim" remains: its highlights query references a non-existent
      -- node type on Neovim 0.11.x and must be suppressed.
      ignore_install = { "vim" },

      highlight = {
        enable  = true,
        disable = function(lang, buf)
          -- FIX (v2.3.10): threshold raised from 100 KB to 500 KB to match the
          -- large_file threshold in autocmds.lua (BufReadPre guard) and the
          -- nvim-ufo provider_selector guard in advanced.lua. Previously a file
          -- between 100 KB–500 KB had treesitter highlights disabled here but
          -- ufo still attempted to attach the treesitter fold provider, causing
          -- a silent mismatch. All three guards now use the same 500 KB limit.
          -- Also checks vim.b.large_file so that the autocmd-set flag is honoured
          -- without a redundant fs_stat call on every BufEnter.
          if vim.b[buf] and vim.b[buf].large_file then return true end
          local max_filesize = 500 * 1024
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

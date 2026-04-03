-- nvim/lua/plugins/specs/treesitter.lua
-- Treesitter configuration for syntax highlighting, indentation, and text objects
--
-- FIX #1: Consolidated ALL nvim-treesitter.configs.setup() calls into the single
-- main spec. Previously, nvim-treesitter-textobjects and nvim-treesitter-refactor
-- each called ts_config.setup() independently — each reinitialisation races
-- against parser compilation and was the true source of the
-- "nvim-treesitter.configs module failed to load" error on first boot.
--
-- FIX #2 (FINAL): nvim-treesitter-refactor has been removed entirely.
-- The plugin ships a plugin/nvim-treesitter-refactor.vim that unconditionally
-- calls init() before the config function runs. Every attempt to suppress it
-- (delete, overwrite, git assume-unchanged) mutates the plugin's git working
-- tree and causes lazy.nvim to block updates permanently.
--
-- All functionality it provided is already covered by existing plugins:
--   highlight_definitions  → vim-illuminate (editor.lua)
--   smart_rename           → vim.lsp.buf.rename, <leader>,r (lsp.lua)
--   navigation (gnd/gnD)   → vim.lsp.buf.definition/references (lsp.lua)
--
-- After saving this file run :Lazy clean to remove the plugin from disk.

return {
  -- ┌─────────────────────────────────────────────────────┐
  -- │               CORE TREESITTER                        │
  -- └─────────────────────────────────────────────────────┘
  {
    "nvim-treesitter/nvim-treesitter",
    build    = ":TSUpdate",
    lazy     = false,
    priority = 100,
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    config = function(_, opts)
      local ok, ts_configs = pcall(require, "nvim-treesitter.configs")
      if not ok then
        vim.notify(
          "nvim-treesitter not ready yet. Run :TSUpdate to install parsers.",
          vim.log.levels.WARN
        )
        return
      end

      ts_configs.setup(opts)

      local ok_hl = pcall(require, "nvim-treesitter.highlight")
      if not ok_hl then
        vim.notify("Treesitter highlight module failed to load", vim.log.levels.WARN)
      end
    end,
    opts = {
      ensure_installed = {
        -- Core languages
        "bash", "c", "cmake", "cpp", "css", "diff",
        "git_rebase", "gitcommit", "go", "html", "http",
        "javascript", "jsdoc", "json", "json5", "jsonc",
        "latex", "lua", "luadoc", "luap",
        "markdown", "markdown_inline",
        "python", "query", "regex", "rust",
        "sql", "toml", "typescript",
        "vim", "vimdoc", "yaml",
        -- Extended languages
        "fortran", "julia", "zig", "ruby",
        "elixir", "heex", "eex",
        "kotlin", "vhdl", "java", "scala",
        "swift", "r", "perl", "php",
        "dart", "vue", "svelte",
      },

      auto_install   = true,
      sync_install   = false,
      ignore_install = { "comment" },

      highlight = {
        enable  = true,
        disable = function(lang, buf)
          local max_filesize = 100 * 1024
          local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
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
      fold    = { enable = true },

      -- ── Textobjects (owned here, not in the sub-plugin spec) ──────────────
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
          swap_next     = { ["<leader>a"] = "@parameter.inner" },
          swap_previous = { ["<leader>A"] = "@parameter.inner" },
        },
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │           TEXTOBJECTS (no config — main owns it)     │
  -- └─────────────────────────────────────────────────────┘
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    lazy = true,
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │               TREESITTER CONTEXT                     │
  -- └─────────────────────────────────────────────────────┘
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
        vim.keymap.set("n", "[c", ctx.go_to_context, { silent = true, desc = "Go to context start" })
      end
    end,
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                  TS PLAYGROUND                       │
  -- └─────────────────────────────────────────────────────┘
  {
    "nvim-treesitter/playground",
    lazy         = true,
    cmd          = { "TSPlaygroundToggle", "TSHighlightCapturesUnderCursor" },
    keys         = {
      { "<leader>xp", "<cmd>TSPlaygroundToggle<CR>",             desc = "TS Playground" },
      { "<leader>xh", "<cmd>TSHighlightCapturesUnderCursor<CR>", desc = "TS Highlight Captures" },
    },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
  },
}

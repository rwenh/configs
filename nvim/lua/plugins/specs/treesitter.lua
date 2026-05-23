-- lua/plugins/specs/treesitter.lua

-- ── Shared capture names ──────────────────────────────────────────────────────

local CAPTURES = {
  func   = "@function.outer",
  func_i = "@function.inner",
  class  = "@class.outer",
  class_i= "@class.inner",
  loop   = "@loop.outer",
  loop_i = "@loop.inner",
  param  = "@parameter.outer",
  param_i= "@parameter.inner",
  cond   = "@conditional.outer",
  cond_i = "@conditional.inner",
}

-- ── Parser categories ─────────────────────────────────────────────────────────

local PARSERS = {
  shell    = { "bash", "diff", "git_rebase", "gitcommit" },
  systems  = { "c", "cmake", "cpp", "rust", "zig", "fortran", "vhdl" },
  web      = { "css", "html", "http",
               "javascript", "jsdoc", "json", "json5", "jsonc",
               "typescript", "tsx", "vue", "svelte" },
  lua      = { "lua", "luadoc", "luap" },
  prose    = { "markdown", "markdown_inline" },
  scripted = { "python", "ruby", "elixir", "heex", "eex" },
  jvm      = { "java", "kotlin" },
  data     = { "sql", "toml", "yaml", "query", "regex" },
  misc     = { "julia", "vimdoc" },
  -- comment: required by todo-comments.nvim (multiline) and noice.nvim.
  meta     = { "comment" },
}

-- ── Neovim version helpers ────────────────────────────────────────────────────
local nvim_011 = vim.fn.has("nvim-0.11") == 1
local nvim_012 = vim.fn.has("nvim-0.12") == 1

return {
  {
    "nvim-treesitter/nvim-treesitter",
    build    = ":TSUpdate",
    lazy     = false,
    priority = 100,
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },

    config = function(_, opts)
      if nvim_011 and not nvim_012 then
        pcall(vim.treesitter.query.set, "vim", "highlights", "")
      end

      local ok, ts_configs = pcall(require, "nvim-treesitter.configs")
      if not ok then
        vim.notify(
          "nvim-treesitter not ready — run :TSUpdate to install parsers.",
          vim.log.levels.WARN
        )
        return
      end
      ts_configs.setup(opts)
    end,

    opts = {
      ensure_installed = vim.iter(vim.tbl_values(PARSERS)):flatten():totable(),

      auto_install   = vim.g.ts_auto_install ~= false
                       and (vim.fn.executable("tree-sitter") == 1
                            or vim.g.ts_auto_install == true),
      sync_install   = false,

      ignore_install = nvim_012 and {} or { "vim" },

      highlight = {
        enable  = true,
        disable = function(lang, buf)
          -- Fast path: already determined to be large.
          if vim.b[buf] and vim.b[buf].large_file then return true end

          if vim.b[buf] ~= nil and vim.b[buf]._ts_size_checked then
            return (vim.b[buf]._ts_large == true) or lang == "vim"
          end

          local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
          local is_large  = ok and stats and stats.size > (500 * 1024)

          -- Cache result: stat fires at most once per buffer lifetime.
          vim.b[buf]._ts_size_checked = true
          vim.b[buf]._ts_large        = is_large and true or false

          if is_large then return true end
          return lang == "vim"
        end,
        additional_vim_regex_highlighting = false,
      },

      indent = {
        enable  = true,
        -- Python indent deferred to vim-python-pep8-indent (python.lua).
        disable = { "python", "yaml", "vue", "html" },
      },

      incremental_selection = {
        enable  = true,
        keymaps = {
          -- In INSERT mode blink.cmp owns <C-space> (open menu).
          -- In NORMAL mode nvim-treesitter owns it (init/expand selection).
          -- There is no functional conflict because the modes are exclusive.
          init_selection    = "<C-space>",
          node_incremental  = "<C-space>",
          scope_incremental = false,
          node_decremental  = "<bs>",
        },
      },

      -- (advanced.lua) is the canonical rainbow implementation.
      -- Enabling it here too would cause double-highlighting.
      rainbow = { enable = false },

      textobjects = {
        select = {
          enable    = true,
          lookahead = true,
          keymaps   = {
            ["af"] = CAPTURES.func,   ["if"] = CAPTURES.func_i,
            ["ac"] = CAPTURES.class,  ["ic"] = CAPTURES.class_i,
            ["al"] = CAPTURES.loop,   ["il"] = CAPTURES.loop_i,
            ["aa"] = CAPTURES.param,  ["ia"] = CAPTURES.param_i,
            ["ai"] = CAPTURES.cond,   ["ii"] = CAPTURES.cond_i,
          },
        },
        move = {
          enable    = true,
          set_jumps = true,
          goto_next_start = {
            ["]f"] = CAPTURES.func,
            ["]c"] = CAPTURES.class,
            ["]a"] = CAPTURES.param,
          },
          goto_next_end = {
            ["]F"] = CAPTURES.func,
            ["]C"] = CAPTURES.class,
            ["]A"] = CAPTURES.param,
          },
          goto_previous_start = {
            ["[f"] = CAPTURES.func,
            ["[c"] = CAPTURES.class,
            ["[a"] = CAPTURES.param,
          },
          goto_previous_end = {
            ["[F"] = CAPTURES.func,
            ["[C"] = CAPTURES.class,
            ["[A"] = CAPTURES.param,
          },
        },
        swap = {
          enable        = true,
          swap_next     = { ["<leader>sa"] = CAPTURES.param_i },
          swap_previous = { ["<leader>sA"] = CAPTURES.param_i },
        },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    lazy = false,
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

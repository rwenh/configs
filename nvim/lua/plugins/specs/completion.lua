-- lua/plugins/specs/completion.lua - blink.cmp completion
--
-- FIX (v2.3.9b):
--   • Clarifying comment added explaining why "cmdline" lives in providers{}
--     but not in sources.default. The two tables serve different scopes:
--     sources.default is the list used for INSERT-mode completion; the cmdline
--     top-level block has its own sources list and pulls from providers by name.
--     Declaring cmdline in providers makes it available to the cmdline block
--     without polluting insert-mode completions. This is intentional and
--     correct; the comment removes the ambiguity that made it look like an
--     omission.

return {
  {
    "saghen/blink.cmp",
    version      = "1.*",
    event        = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "rafamadriz/friendly-snippets",
      "L3MON4D3/LuaSnip",
    },
    config = function(_, opts)
      pcall(function()
        require("luasnip.loaders.from_vscode").lazy_load()
      end)

      local ok = pcall(function() require("blink.cmp").setup(opts) end)
      if not ok then
        vim.notify("blink.cmp setup failed", vim.log.levels.ERROR)
      end
    end,
    opts = {
      keymap = {
        preset        = "default",
        ["<C-Space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-q>"]     = { "hide" },
        ["<CR>"]      = { "accept", "fallback" },
        -- Tab/S-Tab: snippet jumping only. Menu navigation is handled
        -- exclusively by <C-n>/<C-j> and <C-p>/<C-k> below, which are
        -- unambiguous and never conflict with snippet state.
        ["<Tab>"]     = { "snippet_forward", "fallback" },
        ["<S-Tab>"]   = { "snippet_backward", "fallback" },
        -- FIX (v2.3.6): plain select_prev/next only — no "show".
        -- "show" calls "fallback" internally when the menu is open, which
        -- re-invokes native i-^P/i-^N and opens a second competing menu.
        -- Use <C-Space> to open the menu when it is closed.
        ["<C-p>"]     = { "select_prev" },
        ["<C-n>"]     = { "select_next" },
        ["<C-k>"]     = { "select_prev" },
        ["<C-j>"]     = { "select_next" },
        ["<C-b>"]     = { "scroll_documentation_up", "fallback" },
        ["<C-f>"]     = { "scroll_documentation_down", "fallback" },
      },

      appearance = {
        use_nvim_cmp_as_default = false,
        nerd_font_variant       = "mono",
      },

      sources = {
        -- "cmdline" is intentionally absent from sources.default.
        -- sources.default controls INSERT-mode completion only.
        -- The cmdline{} block below has its own sources list and pulls
        -- "cmdline" from the providers table by name. Keeping them separate
        -- prevents the cmdline source from appearing in insert-mode menus
        -- (which would pollute results with ex-command completions mid-code).
        default = { "lsp", "path", "snippets", "buffer" },
        providers = {
          lsp = {
            name               = "LSP",
            module             = "blink.cmp.sources.lsp",
            min_keyword_length = 0,
          },
          path = {
            name   = "Path",
            module = "blink.cmp.sources.path",
          },
          snippets = {
            name               = "Snippets",
            module             = "blink.cmp.sources.snippets",
            min_keyword_length = 2,
          },
          buffer = {
            name               = "Buffer",
            module             = "blink.cmp.sources.buffer",
            min_keyword_length = 2,
            score_offset       = -3,
          },
          -- cmdline declared here so the cmdline{} block can reference it by
          -- name. NOT included in sources.default — insert-mode only uses the
          -- four sources above.
          cmdline = {
            name   = "cmdline",
            module = "blink.cmp.sources.cmdline",
          },
        },
      },

      cmdline = {
        keymap  = { preset = "enter" },
        sources = { "cmdline" },
      },

      snippets = {
        expand = function(snippet)
          local ok_ls, ls = pcall(require, "luasnip")
          if ok_ls then
            pcall(function() ls.lsp_expand(snippet) end)
          else
            pcall(function() vim.snippet.expand(snippet) end)
          end
        end,
        active = function(filter)
          local ok_ls, ls = pcall(require, "luasnip")
          if ok_ls then
            return ls.jumpable(filter and filter.direction or 1)
          end
          return vim.snippet.active(filter)
        end,
        jump = function(direction)
          local ok_ls, ls = pcall(require, "luasnip")
          if ok_ls then
            ls.jump(direction)
          else
            vim.snippet.jump(direction)
          end
        end,
      },

      completion = {
        accept = { auto_brackets = { enabled = true } },
        menu = {
          border = "rounded",
          draw   = {
            treesitter = { "lsp" },
            columns    = {
              { "label", "label_description", gap = 1 },
              { "kind_icon", "kind" },
            },
          },
        },
        documentation = {
          auto_show          = true,
          auto_show_delay_ms = 200,
          window             = { border = "rounded" },
        },
        ghost_text = { enabled = true },
      },

      signature = {
        enabled = true,
        window  = { border = "rounded" },
      },
    },
  },
}

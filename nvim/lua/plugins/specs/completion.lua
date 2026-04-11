-- lua/plugins/specs/completion.lua - blink.cmp completion
--
-- FIX (v2.3.1):
--   • version pinned to "1.*".
--   • min_keyword_length per-source; score_offset on buffer.
--   • cmdline preset switched to "enter".
--
-- FIX (v2.3.2):
--   • cmdline source removed from sources.default. sources.default is the
--     list used during INSERT mode. Including "cmdline" there caused blink to
--     attempt loading the cmdline provider on every InsertEnter, producing a
--     "source not found" warning on some blink versions and polluting insert
--     completions with command-line items. The cmdline source belongs only in
--     the cmdline{} block, which blink activates exclusively on CmdlineEnter.

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
        ["<Tab>"]     = { "snippet_forward", "select_next", "fallback" },
        ["<S-Tab>"]   = { "snippet_backward", "select_prev", "fallback" },
        ["<C-p>"]     = { "select_prev", "fallback" },
        ["<C-n>"]     = { "select_next", "fallback" },
        ["<C-b>"]     = { "scroll_documentation_up", "fallback" },
        ["<C-f>"]     = { "scroll_documentation_down", "fallback" },
      },

      appearance = {
        use_nvim_cmp_as_default = false,
        nerd_font_variant       = "mono",
      },

      sources = {
        -- FIX: "cmdline" removed from the default insert-mode source list.
        -- It belongs only in the cmdline{} block below, which blink uses
        -- exclusively when the command line is open. Listing it here caused
        -- blink to probe the cmdline provider on every InsertEnter.
        default = { "lsp", "path", "snippets", "buffer" },
        providers = {
          lsp = {
            name               = "LSP",
            module             = "blink.cmp.sources.lsp",
            min_keyword_length = 0,   -- trigger on dot, colon, etc.
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
            score_offset       = -3,  -- rank below LSP matches
          },
          -- cmdline provider defined here so the cmdline{} block can reference
          -- it, but NOT listed in sources.default (insert mode).
          cmdline = {
            name   = "cmdline",
            module = "blink.cmp.sources.cmdline",
          },
        },
      },

      cmdline = {
        -- "enter" preset: <CR> confirms selection without double-running.
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

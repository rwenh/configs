-- lua/plugins/specs/completion.lua - blink.cmp completion
--
-- FIX (v2.3.1):
--   • version pinned to "1.*".
--   • min_keyword_length per-source; score_offset on buffer.
--   • cmdline preset switched to "enter".
--
-- FIX (v2.3.2):
--   • cmdline source removed from sources.default.
--
-- FIX (v2.3.4):
--   • <Tab>/<S-Tab> stripped of select_next/select_prev. Tab was fighting
--     snippet jumping and menu navigation simultaneously — when inside a snippet
--     it would jump the placeholder AND move the menu selection, producing
--     unpredictable behaviour. Tab is now snippet-only; menu navigation is
--     handled exclusively by <C-n>/<C-j> (down) and <C-p>/<C-k> (up).
--
-- FIX (v2.3.6):
--   • <C-p>/<C-n>/<C-k>/<C-j> were mapped to { "select_prev/next", "show" }.
--     The "show" action internally calls "fallback" when the completion menu
--     is already visible — this reintroduced the native ins-completion popup
--     (i-^P / i-^N) that the v2.3.4 fix intended to prevent. The comment on
--     those lines claimed "fallback" was removed, but "show" smuggles it back.
--     Fix: map to { "select_prev" } / { "select_next" } only. When the menu
--     is closed these keys are no-ops inside blink — the user should press
--     <C-Space> to open it. This is the unambiguous, conflict-free pattern.

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
        -- FIX (v2.3.6): "show" removed from nav keys. "show" calls "fallback"
        -- internally when the menu is open, which re-invokes native i-^P/i-^N
        -- and opens a second competing menu. Plain select_prev/next is the
        -- correct approach — use <C-Space> to open the menu when closed.
        ["<C-p>"]     = { "select_prev" },
        ["<C-n>"]     = { "select_next" },
        -- Ergonomic aliases: same directional logic as Telescope / fzf.
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
        -- "cmdline" intentionally absent — belongs only in cmdline{} block.
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

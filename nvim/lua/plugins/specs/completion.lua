-- lua/plugins/specs/completion.lua - blink.cmp completion
--
-- FIX (v2.2.5):
--   • version changed from "1.*" to false. blink.cmp uses a rolling semver
--     where 1.x is not yet a published stable tag on most systems — "1.*"
--     resolves to nothing and lazy refuses to install. false = track HEAD
--     of the default branch (stable in practice; blink tags every release).
--   • sources.providers previously declared ONLY "cmdline". The default
--     sources list ("lsp","path","snippets","buffer") references these names
--     as provider keys — blink silently drops any source whose key has no
--     providers entry. All four default sources now have explicit entries so
--     blink actually registers and fires them.
--   • cmdline sources wired correctly via explicit providers.cmdline entry.

return {
  {
    "saghen/blink.cmp",
    version      = false,
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
        default = { "lsp", "path", "snippets", "buffer" },
        -- FIX: all four default sources need explicit provider entries.
        -- Without them blink resolves the source name but finds no module
        -- to back it and silently drops the source from the completion menu.
        -- "cmdline" added so command-line completion also fires correctly.
        providers = {
          lsp = {
            name   = "LSP",
            module = "blink.cmp.sources.lsp",
          },
          path = {
            name   = "Path",
            module = "blink.cmp.sources.path",
          },
          snippets = {
            name   = "Snippets",
            module = "blink.cmp.sources.snippets",
          },
          buffer = {
            name   = "Buffer",
            module = "blink.cmp.sources.buffer",
          },
          cmdline = {
            name   = "cmdline",
            module = "blink.cmp.sources.cmdline",
          },
        },
      },

      cmdline = {
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

-- lua/plugins/specs/completion.lua - Completion via blink.cmp
-- NOTE: blink capabilities are injected into LSP in lsp.lua, not here.
-- Requires Neovim 0.10+ (uses vim.snippet.expand).

return {
  {
    "saghen/blink.cmp",
    -- FIX #6: "v0.*" will stop matching once blink.cmp tags v1.0 — Lazy
    -- would error or fall back to HEAD. Update this pin to "v1.*" when
    -- blink releases its first stable v1. Track: https://github.com/Saghen/blink.cmp/releases
    version = "v0.*",
    event   = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "rafamadriz/friendly-snippets",
    },
    opts = {
      keymap = {
        preset        = "default",
        ["<C-Space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-e>"]     = { "hide" },   -- intentional override of preset's <C-e>
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
      },

      -- NOTE #10: cmdline only completes :commands. No buffer/path sources
      -- in cmdline — intentional for a clean command-line experience.
      -- Add "path" here if you want filepath completion in : commands too.
      cmdline = {
        sources = { "cmdline" },
      },

      snippets = {
        -- FIX #9: vim.snippet.expand requires Neovim 0.10+. This config
        -- targets 0.10+ throughout so this is fine, but noted explicitly.
        -- Do not backport to 0.9.x without replacing this with luasnip expand.
        expand = function(snippet)
          vim.snippet.expand(snippet)
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

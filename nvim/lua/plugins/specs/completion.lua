-- lua/plugins/specs/completion.lua - Completion via blink.cmp (modern, fast)

return {
  -- Remove nvim-cmp entirely; blink.cmp replaces it fully
  {
    "saghen/blink.cmp",
    version = "v0.*",
    event   = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "rafamadriz/friendly-snippets",
      -- Optional: blink compat source for any nvim-cmp sources you still want
      -- "saghen/blink.compat",
    },
    opts = {
      keymap = {
        preset = "default",
        ["<C-Space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-e>"]     = { "hide" },
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
        nerd_font_variant = "mono",
      },

      sources = {
        default = { "lsp", "path", "snippets", "buffer" },
      },

      cmdline = {
        sources = { "cmdline" },
      },

      snippets = {
        expand = function(snippet)
          -- Works with friendly-snippets out of the box
          vim.snippet.expand(snippet)
        end,
      },

      completion = {
        accept = { auto_brackets = { enabled = true } },
        menu = {
          border     = "rounded",
          draw = {
            treesitter = { "lsp" },
            columns    = {
              { "label", "label_description", gap = 1 },
              { "kind_icon", "kind" },
            },
          },
        },
        documentation = {
          auto_show       = true,
          auto_show_delay_ms = 200,
          window = { border = "rounded" },
        },
        ghost_text = { enabled = true },
      },

      signature = {
        enabled = true,
        window  = { border = "rounded" },
      },
    },

    config = function(_, opts)
      require("blink.cmp").setup(opts)

      -- Let LSP know blink provides extended capabilities
      -- (replaces the old cmp_nvim_lsp.default_capabilities() call in lsp.lua)
      vim.lsp.config("*", {
        capabilities = require("blink.cmp").get_lsp_capabilities(),
      })
    end,
  },
}

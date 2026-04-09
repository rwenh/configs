-- lua/plugins/specs/completion.lua - blink.cmp completion
--
-- FIX (v2.3.1):
--   • version pinned to "1.*" using Cargo-style semver that lazy understands.
--     version=false tracked HEAD which has breaking API changes between commits.
--     blink.cmp publishes tagged releases; "1.*" resolves to the latest 1.x
--     stable tag, giving reproducible installs without manual pin maintenance.
--   • min_keyword_length added per-source: LSP fires at 0 chars (dot-trigger),
--     buffer/snippets at 2 chars to avoid noise on short words.
--   • score_offset on buffer source set to -3 so LSP results rank above buffer
--     matches when both are present.
--   • cmdline preset switched to "enter" so <CR> in command mode confirms
--     without also running the command twice.

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
        default = { "lsp", "path", "snippets", "buffer" },
        providers = {
          lsp = {
            name              = "LSP",
            module            = "blink.cmp.sources.lsp",
            min_keyword_length = 0,   -- trigger on dot, colon, etc.
          },
          path = {
            name   = "Path",
            module = "blink.cmp.sources.path",
          },
          snippets = {
            name              = "Snippets",
            module            = "blink.cmp.sources.snippets",
            min_keyword_length = 2,
          },
          buffer = {
            name              = "Buffer",
            module            = "blink.cmp.sources.buffer",
            min_keyword_length = 2,
            score_offset      = -3,   -- rank below LSP matches
          },
          cmdline = {
            name   = "cmdline",
            module = "blink.cmp.sources.cmdline",
          },
        },
      },

      cmdline = {
        -- FIX: "enter" preset — <CR> confirms selection without double-running
        -- the command. The default preset triggers both accept and fallback
        -- which in cmdline mode fires the command a second time.
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

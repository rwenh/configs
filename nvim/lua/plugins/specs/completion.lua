-- lua/plugins/specs/completion.lua — blink.cmp completion
--
--     vim.g.completion_sources_by_ft = {
--       sql      = { "lsp", "dadbod", "buffer" },
--       markdown = { "lsp", "path", "buffer" },
--     }
--
-- AI completion provider:
--   Set vim.g.completion_ai_provider = "copilot" or "codeium" to activate
--   the corresponding blink.cmp source.  The provider plugin must be
--   installed separately.  Supported values: "copilot", "codeium", nil (default).
--

return {
  {
    "saghen/blink.cmp",
    version      = "1.*",
    event        = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "rafamadriz/friendly-snippets",
      "L3MON4D3/LuaSnip",
    },

    opts = {
      keymap = {
        preset        = "default",
        ["<C-Space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-q>"]     = { "hide" },
        ["<CR>"]      = { "accept", "fallback" },
        ["<Tab>"]     = { "snippet_forward",  "fallback" },
        ["<S-Tab>"]   = { "snippet_backward", "fallback" },
        ["<C-p>"]     = { "select_prev" },
        ["<C-n>"]     = { "select_next" },
        ["<C-k>"]     = { "select_prev" },
        ["<C-j>"]     = { "select_next" },
        ["<C-b>"]     = { "scroll_documentation_up",   "fallback" },
        ["<C-f>"]     = { "scroll_documentation_down", "fallback" },
      },

      appearance = {
        use_nvim_cmp_as_default = false,
        nerd_font_variant       = "mono",
      },

      sources = {
        default = { "lsp", "path", "snippets", "buffer" },
        providers = {
          lsp      = { name = "LSP",      module = "blink.cmp.sources.lsp",      min_keyword_length = 0 },
          path     = { name = "Path",     module = "blink.cmp.sources.path" },
          snippets = { name = "Snippets", module = "blink.cmp.sources.snippets", min_keyword_length = 2 },
          buffer   = { name = "Buffer",   module = "blink.cmp.sources.buffer",   min_keyword_length = 2, score_offset = -3 },
          cmdline  = { name = "cmdline",  module = "blink.cmp.sources.cmdline",  min_keyword_length = 0 },
          -- AI provider slots — activated by vim.g.completion_ai_provider at config time.
          copilot  = { name = "Copilot",  module = "blink-copilot",  score_offset = 100, async = true },
          codeium  = { name = "Codeium",  module = "codeium.blink",  score_offset = 100, async = true },
        },
        -- Per-filetype source list; merged from vim.g.completion_sources_by_ft.
        per_filetype = type(vim.g.completion_sources_by_ft) == "table"
          and vim.g.completion_sources_by_ft or {},
      },

      cmdline = { keymap = { preset = "enter" }, sources = { "cmdline" } },

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

      signature = { enabled = true, window = { border = "rounded" } },
    },

    config = function(_, opts)
      local has_ls = pcall(require, "luasnip")
      local ls     = has_ls and require("luasnip") or nil

      if not has_ls then
        vim.notify(
          "[completion] LuaSnip not loaded — snippets will use native vim.snippet.\n"
          .. "Run :Lazy install to ensure LuaSnip is installed.",
          vim.log.levels.WARN
        )
      else
        pcall(function() require("luasnip.loaders.from_vscode").lazy_load() end)
      end

      -- ── AI provider injection ─────────────────────────────────────────────
      local ai = vim.g.completion_ai_provider
      if type(ai) == "string" and ai ~= "" then
        local provider_module = ai == "copilot" and "blink-copilot" or "codeium.blink"
        local has_provider    = pcall(require, provider_module)
        if has_provider then
          if not vim.tbl_contains(opts.sources.default, ai) then
            table.insert(opts.sources.default, ai)
          end
          vim.notify("[completion] AI provider enabled: " .. ai, vim.log.levels.INFO)
        else
          vim.notify(
            "[completion] vim.g.completion_ai_provider = '" .. ai
            .. "' but '" .. provider_module .. "' is not installed.\n"
            .. "Install the provider plugin and run :Lazy install.",
            vim.log.levels.WARN
          )
        end
      end

      opts.snippets = {
        expand = function(snippet)
          if has_ls and ls then pcall(function() ls.lsp_expand(snippet) end)
          else pcall(function() vim.snippet.expand(snippet) end) end
        end,
        active = function(filter)
          if has_ls and ls then return ls.jumpable(filter and filter.direction or 1) end
          return vim.snippet.active(filter)
        end,
        jump = function(direction)
          if has_ls and ls then ls.jump(direction)
          else vim.snippet.jump(direction) end
        end,
      }

      local ok = pcall(function() require("blink.cmp").setup(opts) end)
      if not ok then vim.notify("blink.cmp setup failed", vim.log.levels.ERROR) end
    end,
  },
}

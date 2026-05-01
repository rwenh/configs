-- lua/plugins/specs/completion.lua — blink.cmp completion
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
    config = function(_, opts)
      local has_ls, ls = pcall(require, "luasnip")

      if not has_ls then
        vim.notify(
          "[completion] LuaSnip not loaded — snippet expansion will use native vim.snippet.\n"
          .. "Run :Lazy install to ensure LuaSnip is installed.",
          vim.log.levels.WARN
        )
      else
        pcall(function()
          require("luasnip.loaders.from_vscode").lazy_load()
        end)
      end

      -- Inject the shared ls handle into opts so the closures below can close
      -- over a single resolved value rather than calling pcall(require) 3×.
      opts._ls     = has_ls and ls or nil
      opts._has_ls = has_ls

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

        -- Tab/S-Tab: snippet navigation only.
        -- Menu navigation is handled by <C-n>/<C-j> and <C-p>/<C-k> below,
        -- which never conflict with snippet state.
        ["<Tab>"]   = { "snippet_forward",  "fallback" },
        ["<S-Tab>"] = { "snippet_backward", "fallback" },

        ["<C-p>"] = { "select_prev" },
        ["<C-n>"] = { "select_next" },
        ["<C-k>"] = { "select_prev" },
        ["<C-j>"] = { "select_next" },

        ["<C-b>"] = { "scroll_documentation_up",   "fallback" },
        ["<C-f>"] = { "scroll_documentation_down", "fallback" },
      },

      appearance = {
        use_nvim_cmp_as_default = false,
        nerd_font_variant       = "mono",
      },

      sources = {
        -- "cmdline" intentionally absent from sources.default.
        -- sources.default controls INSERT-mode completion only.
        -- The cmdline{} block below has its own sources list.
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
            -- Set to 0 if buffer suggestions disappear in your setup.
            score_offset       = -3,
          },
          -- Declared here so the cmdline{} block can reference it by name.
          -- NOT in sources.default — insert-mode only uses the four above.
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
          local _ls = rawget(_G, "__blink_ls")   -- fallback; see config() note
          if has_ls and ls then
            pcall(function() ls.lsp_expand(snippet) end)
          else
            pcall(function() vim.snippet.expand(snippet) end)
          end
        end,
        active = function(filter)
          if has_ls and ls then
            return ls.jumpable(filter and filter.direction or 1)
          end
          return vim.snippet.active(filter)
        end,
        jump = function(direction)
          if has_ls and ls then
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
          draw = {
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

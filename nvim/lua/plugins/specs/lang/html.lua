-- lua/plugins/specs/lang/html.lua — HTML development
--

local shared = require("plugins.specs.lang.shared")

return {
  shared.treesitter({ "html" }),

  -- ── LSP: html-lsp — SOLE owner of html server config ──────────────────────

  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      local cfg = {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = {
          provideFormatter = false,
        },
      }

      if vim.fn.executable("vscode-html-language-server") ~= 1 then
        vim.notify("[html] html-lsp not found — run :MasonInstall html-lsp",
          vim.log.levels.WARN)
        return
      end

      if vim.fn.has("nvim-0.11") == 1 then
        pcall(function()
          vim.lsp.config("html", cfg)
          vim.lsp.enable("html")
        end)
      else
        vim.api.nvim_create_autocmd("BufReadPost", {
          pattern  = { "*.html", "*.htmldjango" },
          once     = true,
          group    = vim.api.nvim_create_augroup("HtmlLspCfg", { clear = true }),
          callback = function()
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then pcall(function() lspconfig.html.setup(cfg) end) end
          end,
        })
      end
    end,
  },

}

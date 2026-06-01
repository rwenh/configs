-- lua/plugins/specs/lang/css.lua — CSS development
--

local shared = require("plugins.specs.lang.shared")

return {
  -- ── cssmodules LSP ──────────────────────────────────────────────────────────

  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
        once     = true,
        group    = vim.api.nvim_create_augroup("CssModulesLsp", { clear = true }),
        callback = function()
          if vim.fn.executable("cssmodules-language-server") ~= 1 then
            vim.schedule(function()
              vim.notify(
                "[css] cssmodules-language-server not found — CSS module completion unavailable.\n"
                .. "Install: npm i -g cssmodules-language-server",
                vim.log.levels.DEBUG   -- DEBUG so it doesn't surface unless searched
              )
            end)
            return
          end

          local cfg = {
            init_options = { isCSSModules = true },
            filetypes    = {
              "css", "scss", "less",
              "typescriptreact", "javascriptreact",
            },
          }

          if vim.fn.has("nvim-0.11") == 1 then
            pcall(function()
              vim.lsp.config("cssmodules_ls", cfg)
              vim.lsp.enable("cssmodules_ls")
            end)
          else
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then
              pcall(function() lspconfig.cssmodules_ls.setup(cfg) end)
            end
          end
        end,
      })
    end,
  },

  -- ── Tailwind CSS ───────────────────────────────────────────────────────────

  {
    "luckasRanaringer/tailwind-tools.nvim",
    cond = function()
      return vim.fn.findfile("tailwind.config.js",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.ts",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.cjs", ".;") ~= ""
    end,
    ft = shared.WEB_FT,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      document_color = { enabled = true, kind = "inline" },
      conceal        = { enabled = false },
      server         = { override = false },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("tailwind-tools").setup(opts) end)
      if not ok then
        vim.notify("tailwind-tools setup failed: " .. tostring(err), vim.log.levels.WARN)
      end
    end,
  },

  shared.treesitter({ "css", "scss" }),
}

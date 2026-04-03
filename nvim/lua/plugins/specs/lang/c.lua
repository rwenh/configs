-- lua/plugins/specs/lang/c.lua - C development

return {
  {
    "p00f/clangd_extensions.nvim",
    ft   = { "c", "cpp" },
    opts = {
      ast = {
        role_icons = {
          type                  = "",
          declaration           = "",
          expression            = "",
          specifier             = "",
          statement             = "",
          ["template argument"] = "",
        },
      },
    },
    config = function(_, opts)
      -- RECALIBRATION: Safe setup
      local ok = pcall(function() require("clangd_extensions").setup(opts) end)
      if not ok then
        vim.notify("clangd_extensions setup failed", vim.log.levels.WARN)
        return
      end

      -- Enable inlay hints safely
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("ClangdInlayHints", { clear = true }),
        pattern  = { "*.c", "*.cpp", "*.h", "*.hpp" },
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.name == "clangd" then
            pcall(function() vim.lsp.inlay_hint.enable(true, { bufnr = e.buf }) end)
          end
        end,
      })
    end,
  },
}

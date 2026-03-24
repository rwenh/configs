-- lua/plugins/specs/lang/c.lua - C development
-- clangd_extensions covers both C and C++; cpp.lua delegates here via optional.

return {
  {
    "p00f/clangd_extensions.nvim",
    ft   = { "c", "cpp" },
    opts = {
      -- FIX #1: Removed inlay_hints = { inline = true } — this was the old
      -- clangd_extensions API, silently ignored in current versions.
      -- Neovim 0.10+ has native inlay hint support; enabled via LspAttach below.
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
      require("clangd_extensions").setup(opts)
      -- Enable native inlay hints for C/C++ buffers on LSP attach
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("ClangdInlayHints", { clear = true }),
        pattern  = { "*.c", "*.cpp", "*.h", "*.hpp" },
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.name == "clangd" then
            vim.lsp.inlay_hint.enable(true, { bufnr = e.buf })
          end
        end,
      })
    end,
  },
}

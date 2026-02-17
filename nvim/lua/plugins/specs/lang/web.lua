-- lua/plugins/specs/lang/web.lua
-- Single source of truth for nvim-ts-autotag and emmet
-- Covers: html, css, javascript, typescript, jsx, tsx, xml
-- This replaces the duplicate autotag entries in html.lua, css.lua, javascript.lua, typescript.lua

return {
  -- Auto close/rename HTML tags (ONE definition for all web filetypes)
  {
    "windwp/nvim-ts-autotag",
    event = "InsertEnter",
    ft    = { "html", "xml", "css", "javascript", "typescript", "jsx", "tsx", "typescriptreact", "javascriptreact", "heex", "svelte", "vue" },
    opts  = {
      opts = {
        enable_close         = true,
        enable_rename        = true,
        enable_close_on_slash = false,
      },
    },
  },

  -- Emmet (ONE definition for all web filetypes)
  {
    "mattn/emmet-vim",
    ft   = { "html", "css", "javascript", "jsx", "typescript", "tsx" },
    init = function()
      vim.g.user_emmet_leader_key    = "<C-e>"
      vim.g.user_emmet_install_global = 0
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = { "html", "css", "javascript", "jsx", "typescript", "tsx" },
        callback = function() vim.cmd("EmmetInstall") end,
      })
    end,
  },
}

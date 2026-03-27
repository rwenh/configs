-- lua/plugins/specs/lang/web.lua
-- Single source of truth for nvim-ts-autotag and emmet
-- Covers: html, css, javascript, typescript, jsx, tsx, xml
-- This replaces the duplicate autotag entries in html.lua, css.lua, javascript.lua, typescript.lua

return {
  -- Auto close/rename HTML tags (ONE definition for all web filetypes)
  {
    "windwp/nvim-ts-autotag",
    -- FIX #4: Removed event = "InsertEnter" — ft alone is the correct trigger
    -- for a filetype-scoped plugin. InsertEnter would load it globally on the
    -- first insert in any buffer, defeating lazy loading.
    ft   = { "html", "xml", "css", "javascript", "typescript", "jsx", "tsx",
             "typescriptreact", "javascriptreact", "heex", "svelte", "vue" },
    -- FIX #1: Removed double-nested opts.opts — nvim-ts-autotag's setup()
    -- takes the config at the top level. The nested form passed
    -- { opts = { enable_close = ... } } to setup(), which silently ignored
    -- the inner config and ran with defaults.
    opts = {
      enable_close          = true,
      enable_rename         = true,
      enable_close_on_slash = false,
    },
  },

  -- Emmet (ONE definition for all web filetypes)
  {
    "mattn/emmet-vim",
    -- FIX #3: Replaced tsx/jsx with typescriptreact/javascriptreact — those
    -- are the actual Neovim filetype names. tsx/jsx don't match any buffer.
    ft   = { "html", "css", "javascript", "typescriptreact", "javascriptreact", "typescript" },
    init = function()
      vim.g.user_emmet_leader_key     = "<C-e>"
      vim.g.user_emmet_install_global = 0
      -- FIX #2: Added augroup to prevent autocmd accumulation on reload.
      -- FIX #3: Pattern updated to match actual Neovim filetype names.
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("EmmetInstall", { clear = true }),
        pattern  = { "html", "css", "javascript", "typescriptreact", "javascriptreact", "typescript" },
        callback = function() vim.cmd("EmmetInstall") end,
      })
    end,
  },
}

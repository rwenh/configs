-- ~/.config/nvim/lua/plugins/null-ls.lua
-- null-ls plugin specification (formatters and linters)

return {
  {
    "nvimtools/none-ls.nvim",  -- Updated: none-ls is the maintained fork of null-ls
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "nvim-lua/plenary.nvim" },
    -- Configuration is in config/null-ls.lua
  },
}

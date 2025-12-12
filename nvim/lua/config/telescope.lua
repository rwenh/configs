-- ~/.config/nvim/lua/config/telescope.lua
-- Telescope configuration

local helpers = require("utils.helpers")

helpers.setup_plugin("telescope", function(telescope)
  telescope.setup({
    defaults = {
      prompt_prefix = "   ",
      selection_caret = "  ",
      file_ignore_patterns = { "node_modules", ".git/", "__pycache__/" },
      layout_strategy = "horizontal",
      layout_config = { prompt_position = "top" },
      sorting_strategy = "ascending",
    },
    pickers = {
      find_files = { theme = "dropdown", previewer = false },
      buffers = { theme = "dropdown", previewer = false },
    },
  })
  
  pcall(telescope.load_extension, "fzf")
  pcall(telescope.load_extension, "ui-select")
end)

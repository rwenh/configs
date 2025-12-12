-- ~/.config/nvim/lua/plugins/init.lua
-- Plugin specifications orchestrator

local plugins = {
  require("plugins.lsp"),
  require("plugins.completion"),
  require("plugins.treesitter"),
  require("plugins.editor"),
  require("plugins.git"),
  require("plugins.dap"),
  require("plugins.testing"),
  require("plugins.ui"),
  require("plugins.null-ls"),  -- Added: null-ls plugins
}

-- Flatten nested tables
local flat_plugins = {}
for _, plugin_group in ipairs(plugins) do
  if type(plugin_group) == "table" then
    if plugin_group[1] then  -- It's a plugin spec
      table.insert(flat_plugins, plugin_group)
    else  -- It's an array of specs
      for _, spec in ipairs(plugin_group) do
        table.insert(flat_plugins, spec)
      end
    end
  end
end

require("lazy").setup(flat_plugins, {
  defaults = { lazy = true },
  install = { colorscheme = { "solarized" } },
  checker = { enabled = true, notify = false, frequency = 3600 },
  change_detection = { enabled = true, notify = false },
  performance = {
    cache = { enabled = true },
    reset_packpath = true,
    rtp = {
      disabled_plugins = {
        "gzip", "matchit", "matchparen", "netrwPlugin",
        "tarPlugin", "tohtml", "tutor", "zipPlugin"
      },
    },
  },
  ui = {
    size = { width = 0.8, height = 0.8 },
    border = "rounded",
    title = "Neovim IDE Plugin Manager",
  },
})

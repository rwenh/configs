-- ~/.config/nvim/lua/plugins/init.lua

local plugins = {
  require("plugins.lsp"),
  require("plugins.completion"),
  require("plugins.treesitter"),
  require("plugins.editor"),
  require("plugins.git"),
  require("plugins.dap"),
  require("plugins.testing"),
  require("plugins.ui"),
  require("plugins.null-ls"),
}

-- FIXED: Recursive flattening for nested tables
local function flatten_plugins(tbl, result)
  result = result or {}

  for _, item in ipairs(tbl) do
    if type(item) == "table" then
      -- Check if it's a plugin spec (has string at index 1)
      if type(item[1]) == "string" then
        table.insert(result, item)
      else
        -- Recursively flatten nested tables
        flatten_plugins(item, result)
      end
    end
  end

  return result
end

local flat_plugins = flatten_plugins(plugins)

require("lazy").setup(flat_plugins, {
  defaults = {
    lazy = true,
    version = false,
  },
  install = {
    colorscheme = { "solarized" },
    missing = true,
  },
  checker = {
    enabled = true,
    notify = false,
    frequency = 3600,
  },
  change_detection = {
    enabled = true,
    notify = false,
  },
  performance = {
    cache = { enabled = true },
    reset_packpath = true,
    rtp = {
      reset = true,
      paths = {},
      -- FIXED: Proper disabled plugins list
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        "netrwPlugin",  -- We use nvim-tree
      },
    },
  },
  ui = {
    size = { width = 0.8, height = 0.8 },
    wrap = true,
    border = "rounded",
    title = "Neovim IDE Plugin Manager",
    icons = {
      cmd = " ",
      config = "",
      event = "",
      ft = " ",
      init = " ",
      import = " ",
      keys = " ",
      lazy = "󰂠 ",
      loaded = "●",
      not_loaded = "○",
      plugin = " ",
      runtime = " ",
      require = "󰢱 ",
      source = " ",
      start = "",
      task = "✓ ",
      list = {
        "●",
        "➜",
        "★",
        "‒",
      },
    },
  },
  diff = {
    cmd = "git",
  },
  profiling = {
    loader = false,
    require = false,
  },
})

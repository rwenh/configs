-- ~/.config/nvim/init.lua
-- Modular Neovim IDE - Entry Point
-- Version: 2.0 (Based on your v6.6 monolithic config)

-- =====================================================
-- BOOTSTRAP & GLOBAL STATE
-- =====================================================
if vim.loader then vim.loader.enable() end

_G.nvim_ide = {
  version = "2.0-modular",
  root = vim.fn.stdpath("config"),
  data = vim.fn.stdpath("data"),
  cache = vim.fn.stdpath("cache"),
  debug = vim.env.NVIM_DEBUG == "1",
  security_mode = vim.env.NVIM_SECURITY == "1",
  failed = {},
  startup_time = vim.uv.hrtime(),
  language_servers = {},
  dap_adapters = {},
  os_type = vim.uv.os_uname().sysname,
}

-- =====================================================
-- MODULE LOADER
-- =====================================================
local function safe_require(mod)
  local ok, result = pcall(require, mod)
  if not ok then
    _G.nvim_ide.failed[mod] = result
    if _G.nvim_ide.debug then
      vim.notify("Failed: " .. mod, vim.log.levels.WARN)
    end
    return nil
  end
  return result
end

-- =====================================================
-- LOAD CORE MODULES
-- =====================================================
safe_require("core.options")      -- Vim options
safe_require("core.lazy")         -- Plugin manager bootstrap
safe_require("core.theme")        -- Colorscheme system
safe_require("plugins")           -- Plugin specifications
safe_require("config.lsp")        -- LSP configuration
safe_require("config.completion") -- Completion setup
safe_require("config.treesitter") -- Syntax highlighting
safe_require("config.telescope")  -- Fuzzy finder
safe_require("config.git")        -- Git integration
safe_require("config.dap")        -- Debugging
safe_require("config.ui")         -- UI plugins
safe_require("config.testing")    -- Neotest configuration
safe_require("config.null-ls")    -- Formatters and linters (NOW LOADED)
safe_require("core.keymaps")      -- All keybindings
safe_require("core.autocmds")     -- Autocommands
safe_require("core.commands")     -- Custom commands
safe_require("utils.runner")      -- Code execution
safe_require("utils.platform")    -- Platform-specific

-- =====================================================
-- STARTUP MESSAGE
-- =====================================================
vim.defer_fn(function()
  local lazy_ok, lazy = pcall(require, "lazy")
  local startup_time = (vim.uv.hrtime() - _G.nvim_ide.startup_time) / 1e6
  
  local plugin_count = 0
  if lazy_ok then
    local stats = lazy.stats()
    plugin_count = stats.loaded
  end
  
  vim.notify(
    string.format("⚡ IDE ready: %.1fms | %d plugins", 
      startup_time, plugin_count),
    vim.log.levels.INFO,
    { title = "Neovim IDE v2.0" }
  )
end, 100)

-- =====================================================
-- DIRECTORY STRUCTURE REFERENCE
-- =====================================================
--[[
~/.config/nvim/
├── init.lua (THIS FILE)
├── lua/
│   ├── core/
│   │   ├── options.lua      - Vim options
│   │   ├── lazy.lua         - Plugin manager
│   │   ├── theme.lua        - Colorscheme
│   │   ├── keymaps.lua      - All keybindings
│   │   ├── autocmds.lua     - Autocommands
│   │   └── commands.lua     - Custom commands
│   ├── plugins/
│   │   ├── init.lua         - Plugin orchestrator
│   │   ├── lsp.lua          - LSP plugins
│   │   ├── completion.lua   - Completion plugins
│   │   ├── treesitter.lua   - Treesitter
│   │   ├── editor.lua       - File tree, telescope
│   │   ├── git.lua          - Git plugins
│   │   ├── dap.lua          - Debugger plugins
│   │   ├── testing.lua      - Neotest
│   │   └── ui.lua           - UI plugins
│   ├── config/
│   │   ├── lsp.lua          - LSP setup
│   │   ├── completion.lua   - nvim-cmp
│   │   ├── treesitter.lua   - Treesitter config
│   │   ├── telescope.lua    - Telescope
│   │   ├── git.lua          - Gitsigns
│   │   ├── dap.lua          - Debugger
│   │   ├── testing.lua      - Neotest
│   │   ├── null-ls.lua      - Formatters/Linters
│   │   └── ui.lua           - UI configs
│   └── utils/
│       ├── runner.lua       - Code execution
│       ├── platform.lua     - Clipboard
│       └── helpers.lua      - Utilities

BENEFITS:
  ✓ Single responsibility per file
  ✓ Easy to disable features
  ✓ Hot-reload individual modules
  ✓ Better git diffs
  ✓ Team collaboration friendly
]]

-- ~/.config/nvim/lua/core/commands.lua
-- Custom commands

local helpers = require("utils.helpers")

-- Health check command
vim.api.nvim_create_user_command("Health", function(opts)
  local health = {}
  
  health["Neovim"] = vim.version() and tostring(vim.version()) or "Unknown"
  health["OS"] = vim.uv.os_uname().sysname
  health["Clipboard"] = vim.fn.has("clipboard") == 1 and "Available" or "Not available"
  health["Git"] = helpers.command_exists("git") and "Available" or "Not available"
  
  local lsp_clients = vim.lsp.get_clients()
  health["LSP Clients"] = #lsp_clients > 0 and (#lsp_clients .. " active") or "None"
  
  local lazy_ok, lazy = pcall(require, "lazy")
  if lazy_ok then
    local stats = lazy.stats()
    health["Plugins"] = string.format("%d/%d loaded", stats.loaded, stats.count)
    health["Startup Time"] = string.format("%.2fms", stats.startuptime)
  end
  
  local memory_mb = collectgarbage("count") / 1024
  health["Memory Usage"] = string.format("%.1fMB", memory_mb)
  
  health["Theme"] = string.format("Solarized (%s)", vim.o.background)
  
  if opts.bang then
    print(vim.json.encode(health))
    return
  end
  
  print("=== Neovim IDE Health Check ===")
  for key, value in pairs(health) do
    print(string.format("%-15s: %s", key, value))
  end
  
  if next(_G.nvim_ide.failed) then
    print("\nFailed Modules:")
    for module, _ in pairs(_G.nvim_ide.failed) do
      print("  - " .. module)
    end
  end
end, { desc = "Show IDE health status", bang = true })

-- Format command
vim.api.nvim_create_user_command("Format", function(opts)
  local range = nil
  if opts.range > 0 then
    range = { start = { opts.line1, 0 }, ["end"] = { opts.line2, 0 } }
  end
  vim.lsp.buf.format({ async = true, range = range })
end, { range = true, desc = "Format buffer or range" })

-- Project root command
vim.api.nvim_create_user_command("ProjectRoot", function()
  local root = helpers.find_project_root()
  vim.cmd("cd " .. root)
  vim.notify("Changed to: " .. root)
end, { desc = "Navigate to project root" })

-- Cleanup command
vim.api.nvim_create_user_command("CleanUp", function()
  collectgarbage("collect")
  local memory_mb = collectgarbage("count") / 1024
  vim.notify("Cleanup completed - Memory: " .. string.format("%.1fMB", memory_mb))
end, { desc = "Clean up memory" })

-- Debug command
vim.api.nvim_create_user_command("DebugIDE", function()
  local debug_info = {
    startup_time = string.format("%.2fms", (vim.uv.hrtime() - _G.nvim_ide.startup_time) / 1e6),
    memory = string.format("%.2fMB", collectgarbage("count") / 1024),
    failed_modules = vim.tbl_count(_G.nvim_ide.failed),
    lsp_clients = #vim.lsp.get_clients(),
    buffers = #vim.api.nvim_list_bufs(),
  }
  
  print("=== IDE Debug Information ===")
  for key, value in pairs(debug_info) do
    print(string.format("%-15s: %s", key, value))
  end
end, { desc = "Show debug information" })

-- Theme commands
vim.api.nvim_create_user_command("SolarizedLight", function()
  vim.o.background = "light"
  pcall(vim.cmd.colorscheme, "solarized")
  require("core.theme").apply_highlights()
end, { desc = "Set Solarized Light" })

vim.api.nvim_create_user_command("SolarizedDark", function()
  vim.o.background = "dark"
  pcall(vim.cmd.colorscheme, "solarized")
  require("core.theme").apply_highlights()
end, { desc = "Set Solarized Dark" })

-- ~/.config/nvim/lua/core/health.lua

local M = {}

function M.check()
  vim.health.start("Neovim IDE Core")

  -- Check Neovim version
  local nvim_version = vim.version()
  if nvim_version.minor >= 10 then
    vim.health.ok(string.format("Neovim %d.%d.%d", nvim_version.major, nvim_version.minor, nvim_version.patch))
  else
    vim.health.warn(
      string.format("Neovim %d.%d.%d detected", nvim_version.major, nvim_version.minor, nvim_version.patch),
      "Some features require Neovim 0.10+"
    )
  end

  -- Check clipboard
  if vim.fn.has("clipboard") == 1 then
    vim.health.ok("Clipboard support available")
  else
    vim.health.error(
      "Clipboard support not available",
      "Install xclip, xsel, wl-clipboard, or other clipboard provider"
    )
  end

  -- Check essential commands
  local commands = {
    { cmd = "git", desc = "Git version control" },
    { cmd = "rg", desc = "Ripgrep for telescope" },
    { cmd = "fd", desc = "Fast file finder" },
  }

  for _, item in ipairs(commands) do
    if vim.fn.executable(item.cmd) == 1 then
      vim.health.ok(item.cmd .. " found")
    else
      vim.health.warn(
        item.cmd .. " not found",
        item.desc .. " - install for better experience"
      )
    end
  end

  -- Check failed modules
  if _G.nvim_ide and next(_G.nvim_ide.failed) then
    vim.health.start("Failed Modules")
    for mod, info in pairs(_G.nvim_ide.failed) do
      vim.health.error("Failed to load: " .. mod, info.error)
    end
  else
    vim.health.ok("All core modules loaded successfully")
  end

  -- Check plugin status
  local lazy_ok, lazy = pcall(require, "lazy")
  if lazy_ok then
    vim.health.start("Plugin Manager")
    local stats = lazy.stats()
    vim.health.ok(string.format("%d/%d plugins loaded", stats.loaded, stats.count))
    vim.health.info(string.format("Startup time: %.2fms", stats.startuptime))

    -- Check for plugin errors
    local errors = {}
    for _, plugin in pairs(lazy.plugins()) do
      if plugin._.errors then
        table.insert(errors, plugin.name)
      end
    end

    if #errors > 0 then
      vim.health.error(
        "Some plugins have errors",
        "Plugins with errors: " .. table.concat(errors, ", ")
      )
    end
  end

  -- Check memory usage
  if _G.nvim_ide then
    vim.health.start("Memory Usage")
    local memory_mb = collectgarbage("count") / 1024
    local limit = _G.nvim_ide.memory_limit_mb or 1024
    local percentage = (memory_mb / limit) * 100

    if percentage < 80 then
      vim.health.ok(string.format("Memory: %.1fMB / %dMB (%.1f%%)", memory_mb, limit, percentage))
    elseif percentage < 95 then
      vim.health.warn(
        string.format("Memory: %.1fMB / %dMB (%.1f%%)", memory_mb, limit, percentage),
        "Memory usage is high, consider running :CleanUp"
      )
    else
      vim.health.error(
        string.format("Memory: %.1fMB / %dMB (%.1f%%)", memory_mb, limit, percentage),
        "Memory usage is critical, run :CleanUp immediately"
      )
    end
  end

  -- Check LSP status
  vim.health.start("LSP Servers")
  local lsp_clients = vim.lsp.get_clients()
  if #lsp_clients > 0 then
    vim.health.ok(string.format("%d LSP client(s) active", #lsp_clients))
    for _, client in pairs(lsp_clients) do
      vim.health.info("  • " .. client.name)
    end
  else
    vim.health.info("No LSP clients currently active")
  end

  -- Check Treesitter
  local ts_ok, ts_parsers = pcall(require, "nvim-treesitter.parsers")
  if ts_ok then
    vim.health.start("Treesitter")
    local installed = {}
    for lang, _ in pairs(ts_parsers.get_parser_configs()) do
      if ts_parsers.has_parser(lang) then
        table.insert(installed, lang)
      end
    end
    vim.health.ok(string.format("%d parser(s) installed", #installed))
  end

  -- Check DAP adapters
  if _G.nvim_ide and _G.nvim_ide.dap_adapters then
    vim.health.start("Debug Adapters")
    local adapters = {}
    for lang, status in pairs(_G.nvim_ide.dap_adapters) do
      if status then
        table.insert(adapters, lang)
      end
    end

    if #adapters > 0 then
      vim.health.ok("DAP adapters configured: " .. table.concat(adapters, ", "))
    else
      vim.health.info("No DAP adapters configured")
    end
  end

  -- Check formatters (null-ls)
  local null_ls_ok, null_ls = pcall(require, "null-ls")
  if null_ls_ok then
    vim.health.start("Formatters & Linters")
    local sources = null_ls.get_sources()
    if #sources > 0 then
      vim.health.ok(string.format("%d null-ls source(s) active", #sources))
      local by_type = {}
      for _, source in ipairs(sources) do
        local method = source.method
        by_type[method] = (by_type[method] or 0) + 1
      end
      for method, count in pairs(by_type) do
        vim.health.info(string.format("  • %s: %d", method, count))
      end
    else
      vim.health.info("No null-ls sources active")
    end
  end

  -- Performance tips
  vim.health.start("Performance Tips")
  if stats and stats.startuptime > 100 then
    vim.health.info(
      "Startup time is above 100ms",
      "Run :Lazy profile to identify slow plugins"
    )
  end
end

return M

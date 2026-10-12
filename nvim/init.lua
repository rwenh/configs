-- ~/.config/nvim/init.lua — v2.6.1 entry point
--
-- Load order: bootstrap → options → autocmds → keymaps → commands → lazy/plugins → theme → highlights

-- 0. Byte-compile + cache Lua modules (Neovim 0.9+). Must be first. lazy.nvim
--    detects it and stands down its own cache.
if vim.loader then
  vim.loader.enable()
end

-- ── Poison-proof require ──────────────────────────────────────────────────────
-- LuaJIT leaves a sentinel in package.loaded after a FAILED require, so the next
-- require of that module reports "loop or previous error loading module 'X'" and
-- hides the real error. safe_require() clears sentinels under the given prefixes,
-- retries once, and returns the real error (with a traceback) if it still fails.
local function clear_poison(prefixes)
  local cleared = {}
  for name, v in pairs(package.loaded) do
    if type(name) == "string" and type(v) == "number" then
      for _, p in ipairs(prefixes) do
        if name == p or name:sub(1, #p + 1) == p .. "." then
          package.loaded[name] = nil
          table.insert(cleared, name)
        end
      end
    end
  end
  return cleared
end

local function safe_require(mod, prefixes)
  local ok, err = xpcall(require, debug.traceback, mod)
  if ok then
    return true
  end
  if tostring(err):find("loop or previous error loading module", 1, true) then
    local cleared = clear_poison(prefixes or { mod })
    local ok2, err2 = xpcall(require, debug.traceback, mod)
    if ok2 then
      return true
    end
    return false,
      string.format(
        "%s\n(cleared poisoned entries: %s)\nFIRST REAL ERROR:\n%s",
        tostring(err),
        #cleared > 0 and table.concat(cleared, ", ") or "none",
        tostring(err2)
      )
  end
  return false, err
end

-- 1. Bootstrap lazy.nvim + leader keys + version stamp (must be first)
require("core.bootstrap")

-- 2. Vim options (no plugins needed)
require("core.options")

-- 3. Autocommands
require("core.autocmds")

-- 4. Global keymaps
require("core.keymaps")

-- 5. User commands
require("core.commands")

-- 6. Plugin manager + all plugin specs (core.lazy; was lua/plugins/init.lua)
local _plugins_ok, _plugins_err = safe_require("core.lazy", { "core.lazy", "plugins" })
if not _plugins_ok then
  vim.notify(
    "[init.lua] core.lazy failed to load:\n"
      .. tostring(_plugins_err)
      .. "\n\nTroubleshooting:\n"
      .. "  1. Delete ~/.local/share/nvim and ~/.cache/nvim\n"
      .. "  2. Restart Neovim — lazy.nvim will reinstall\n"
      .. "  3. Run :MasonInstallAll\n",
    vim.log.levels.ERROR
  )
end

-- 7. Theme (after plugins so colorscheme plugins are available)
local ok_theme, theme_err = pcall(function()
  require("core.theme").setup()
end)
if not ok_theme then
  vim.notify(
    "[init.lua] theme.setup() failed: " .. tostring(theme_err) .. "\nFalling back to built-in 'default' colorscheme.",
    vim.log.levels.WARN
  )
  pcall(vim.cmd.colorscheme, "default")
end

-- 8. Highlight overrides
local ok_hl, hl_err = pcall(function()
  require("core.highlights").apply()
end)
if not ok_hl then
  vim.notify("[init.lua] highlights.apply() failed: " .. tostring(hl_err), vim.log.levels.WARN)
end

-- 9. Startup stats (only when plugins loaded successfully)
if _plugins_ok then
  vim.api.nvim_create_autocmd("User", {
    pattern = "LazyDone",
    once = true,
    callback = function()
      -- Cross-check lspconfig ↔ Mason naming drift (logs at DEBUG level only)
      pcall(function()
        require("core.util.packages").validate()
      end)
      pcall(function()
        require("core.util.packages").validate_dap()
      end)

      -- Flag names, types and defaults live in lua/core/flags.lua (single source of truth).
      local ok_flags, flags = pcall(require, "core.flags")
      local flag_issues = ok_flags and flags.type_issues() or {}
      if #flag_issues > 0 then
        vim.notify(
          "[init] Escape hatch type mismatch — these flags will silently use defaults:\n"
            .. table.concat(flag_issues, "\n"),
          vim.log.levels.WARN
        )
      end

      local ok_lazy, lazy = pcall(require, "lazy")
      if ok_lazy then
        local s = lazy.stats()
        vim.notify(string.format("⚡ %d plugins loaded in %.2fms", s.count, s.startuptime), vim.log.levels.INFO)
      end
    end,
  })
end

-- NOTE: vim.g.nvim_ide_version is set in core/bootstrap.lua (step 1).

-- ~/.config/nvim/init.lua — v2.6.1 entry point
--
-- Load order: bootstrap → options → autocmds → keymaps → commands → lazy/plugins → theme → highlights
--
-- Each step is guarded. A failure is reported by module name and startup carries on, so one broken
-- module cannot leave the editor without keymaps or plugins.

-- Leader keys first: keymaps stay correct even if core.bootstrap fails. bootstrap sets them again.
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- 0. Byte-compile and cache Lua modules (Neovim 0.9+). lazy.nvim stands down its own cache when this is on.
if vim.loader then
  vim.loader.enable()
end

-- ── Poison-proof require ──────────────────────────────────────────────────────
-- LuaJIT leaves a sentinel in package.loaded after a FAILED require, so the next require of that module
-- reports "loop or previous error loading module 'X'" and hides the real error. safe_require() clears the
-- sentinels under the given prefixes, retries once, and returns the real error (with a traceback) if it
-- still fails.
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

-- ── Steps ─────────────────────────────────────────────────────────────────────

--- Require one module. A failure is reported by name, and startup carries on.
---@param mod      string
---@param prefixes string[]?
---@return boolean ok
local function step(mod, prefixes)
  local ok, err = safe_require(mod, prefixes)
  if not ok then
    vim.notify(("[init] %s failed to load:\n%s"):format(mod, tostring(err)), vim.log.levels.ERROR)
  end
  return ok
end

-- 1–5. Core modules
step("core.bootstrap")
step("core.options")
step("core.autocmds")
step("core.keymaps")
step("core.commands")

-- 6. Plugin manager and every plugin spec (core.lazy)
local plugins_ok = step("core.lazy", { "core.lazy", "plugins" })
if plugins_ok then
  local ok_setup, setup_err = pcall(function()
    require("core.lazy").setup()
  end)
  if not ok_setup then
    plugins_ok = false
    vim.notify(
      "[init] lazy.nvim setup failed:\n"
        .. tostring(setup_err)
        .. "\n\nTroubleshooting:\n"
        .. "  1. Delete ~/.local/share/nvim and ~/.cache/nvim\n"
        .. "  2. Restart Neovim — lazy.nvim will reinstall\n"
        .. "  3. Run :MasonInstallAll\n",
      vim.log.levels.ERROR
    )
  end
end

-- 7. Theme (after plugins, so colorscheme plugins exist)
local ok_theme, theme_err = pcall(function()
  require("core.theme").setup()
end)
if not ok_theme then
  vim.notify(
    "[init] theme.setup() failed: " .. tostring(theme_err) .. "\nFalling back to built-in 'default' colorscheme.",
    vim.log.levels.WARN
  )
  pcall(vim.cmd.colorscheme, "default")
end

-- 8. Highlight overrides
local ok_hl, hl_err = pcall(function()
  require("core.highlights").apply()
end)
if not ok_hl then
  vim.notify("[init] highlights.apply() failed: " .. tostring(hl_err), vim.log.levels.WARN)
end

-- 9. Startup checks and stats (only when plugins loaded)
if plugins_ok then
  vim.api.nvim_create_autocmd("User", {
    pattern = "LazyDone",
    once = true,
    callback = function()
      -- Naming drift between lspconfig and Mason (logged at DEBUG only)
      pcall(function()
        require("core.util.packages").validate()
      end)
      pcall(function()
        require("core.util.packages").validate_dap()
      end)

      -- Flag names, types and defaults live in lua/core/flags.lua
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

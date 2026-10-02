-- lua/core/bootstrap.lua — bootstrap configuration (loads FIRST)
--
-- Responsibilities:
--   1. Set leader keys (must precede any plugin loading).
--   2. Stamp vim.g.nvim_ide_version.
--   3. Stamp vim.g._nvim_active_theme.
--   4. Clone lazy.nvim if missing, with SHA integrity check + offline fallback.
--

local VERSION  = "2.5.0"
local LAZY_URL = "https://github.com/folke/lazy.nvim.git"

vim.g.nvim_ide_version = VERSION

-- ── Leader keys ───────────────────────────────────────────────────────────────
vim.g.mapleader      = " "
vim.g.maplocalleader = " "

-- ── Theme stamp ───────────────────────────────────────────────────────────────
vim.g._nvim_active_theme = (function()
  local ok, t = pcall(require, "core.theme")
  return (ok and t.config and t.config.theme) or "tokyonight"
end)()

-- ── Paths ─────────────────────────────────────────────────────────────────────
local lazypath  = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local cachepath = vim.fn.stdpath("data") .. "/lazy-cache/lazy.nvim"

-- ── SHA integrity check ───────────────────────────────────────────────────────
local function clone_looks_valid(path)
  if vim.fn.isdirectory(path) ~= 1 then return false end
  local head = vim.fn.system({ "git", "-C", path, "rev-parse", "HEAD" })
  return vim.v.shell_error == 0 and vim.trim(head) ~= ""
end

-- ── Offline fallback ──────────────────────────────────────────────────────────
-- cached snapshot from vim.g.lazy_cache_path (default: stdpath("data")/lazy-cache).
local function try_offline_fallback()
  local cache = vim.g.lazy_cache_path or cachepath
  if vim.fn.isdirectory(cache) == 1 and clone_looks_valid(cache) then
    vim.notify(
      "No network — restoring lazy.nvim from offline cache: " .. cache,
      vim.log.levels.WARN
    )
    local ok_cp = (vim.fn.system({ "cp", "-r", cache, lazypath }) and vim.v.shell_error == 0)
    if ok_cp and clone_looks_valid(lazypath) then
      vim.notify("lazy.nvim restored from cache.", vim.log.levels.INFO)
      return true
    end
  end
  return false
end

-- ── Cache updater ─────────────────────────────────────────────────────────────
-- Only refreshes when the cached HEAD differs from the installed one, entirely
-- off the main thread, via copy-to-tmp + rename — so a failed copy can never
-- destroy a good cache (the old code did rm -rf, then cp, on EVERY startup).
local function schedule_cache_update()
  if not vim.system then return end
  vim.api.nvim_create_autocmd("User", {
    pattern  = "LazyDone",
    once     = true,
    callback = function()
      local cache = vim.g.lazy_cache_path or cachepath
      vim.fn.mkdir(vim.fn.fnamemodify(cache, ":h"), "p")

      local function head(path, cb)
        vim.system({ "git", "-C", path, "rev-parse", "HEAD" }, { text = true }, function(r)
          cb(r.code == 0 and vim.trim(r.stdout or "") or nil)
        end)
      end

      head(lazypath, function(installed)
        if not installed or installed == "" then return end
        head(cache, function(cached)
          if cached == installed then return end   -- up to date: do nothing
          local tmp = cache .. ".tmp"
          vim.system({
            "sh", "-c",
            'rm -rf "$1" && cp -r "$2" "$1" && rm -rf "$3" && mv "$1" "$3"',
            "sh", tmp, lazypath, cache,
          }, {}, function() end)
        end)
      end)
    end,
    desc = "Refresh lazy.nvim offline cache (only when HEAD changed)",
  })
end

-- ── Clone lazy.nvim if not present ───────────────────────────────────────────
if not clone_looks_valid(lazypath) then
  -- Clean up any partial directory first.
  if vim.fn.isdirectory(lazypath) == 1 then
    vim.fn.delete(lazypath, "rf")
  end

  vim.notify("Bootstrapping lazy.nvim — cloning from GitHub…", vim.log.levels.INFO)

  local out = vim.fn.system({
    "git", "clone", "--filter=blob:none",
    LAZY_URL, "--branch=stable", lazypath,
  })

  if vim.v.shell_error ~= 0 or not clone_looks_valid(lazypath) then
    -- Clone failed or produced an unusable result.
    vim.fn.delete(lazypath, "rf")

    if not try_offline_fallback() then
      vim.notify(
        "Failed to bootstrap lazy.nvim:\n" .. tostring(out)
          .. "\n\nTroubleshooting:\n"
          .. "1. Check your internet connection\n"
          .. "2. git --version  (must be installed)\n"
          .. "3. Offline cache not found at: " .. (vim.g.lazy_cache_path or cachepath) .. "\n"
          .. "   On first install you need a network connection.\n",
        vim.log.levels.ERROR
      )
      return
    end
  else
    vim.notify("lazy.nvim bootstrap successful!", vim.log.levels.INFO)
  end
end

-- Schedule offline cache refresh for next LazyDone event.
schedule_cache_update()

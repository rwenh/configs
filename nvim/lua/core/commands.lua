-- lua/core/commands.lua — user commands

local cmd = vim.api.nvim_create_user_command
if vim.g.disable_autoformat == nil then vim.g.disable_autoformat = false end

local MASON_PACKAGES = require("core.util.packages")

local function copy_path(expand_fmt, label)
  return function()
    local p = vim.fn.expand(expand_fmt)
    if p == "" then vim.notify("[" .. label .. "] no file path available", vim.log.levels.WARN); return end
    pcall(function() vim.fn.setreg("+", p); vim.notify("[" .. label .. "] copied: " .. p) end)
  end
end

local function make_toggle(wo_key, display_name)
  return function()
    pcall(function()
      vim.wo[wo_key] = not vim.wo[wo_key]
      vim.notify(display_name .. ": " .. (vim.wo[wo_key] and "on" or "off"))
    end)
  end
end

-- ── :Health ───────────────────────────────────────────────────────────────────
cmd("Health", function()
  local ok_ver, version = pcall(vim.version)
  local ok_os,  os_info = pcall(vim.uv.os_uname)
  local ok_lsp, clients = pcall(vim.lsp.get_clients)
  local ok_mem, mem     = pcall(function() return collectgarbage("count") / 1024 end)
  local lines = {
    "=== Neovim Health ===",
    string.format("%-12s: %s", "nvim",    ok_ver and string.format("v%d.%d.%d", version.major, version.minor, version.patch) or "unknown"),
    string.format("%-12s: %s", "os",      ok_os  and os_info.sysname  or "unknown"),
    string.format("%-12s: %s", "lsp",     ok_lsp and (#clients > 0 and string.format("%d active", #clients) or "none") or "unknown"),
    string.format("%-12s: %s", "memory",  ok_mem and string.format("%.1fMB", mem) or "unknown"),
    string.format("%-12s: %s", "ide ver", tostring(vim.g.nvim_ide_version or "unknown")),
  }
  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO)
end, { desc = "Show health status" })

-- ── :Format ───────────────────────────────────────────────────────────────────
cmd("Format", function(opts)
  local ok, conform = pcall(require, "conform")
  if not ok then vim.notify("conform.nvim not available", vim.log.levels.ERROR); return end
  if opts.range > 0 then
    local last_lines = vim.api.nvim_buf_get_lines(0, opts.line2 - 1, opts.line2, false)
    local end_col    = #(last_lines[1] or "")
    pcall(function()
      conform.format({ lsp_format = "fallback", range = { start = { opts.line1, 0 }, ["end"] = { opts.line2, end_col } } })
    end)
  else
    pcall(function() conform.format({ lsp_format = "fallback" }) end)
  end
end, { range = true, desc = "Format file or range" })

-- ── :ProjectRoot ──────────────────────────────────────────────────────────────
cmd("ProjectRoot", function()
  local ok, path_util = pcall(require, "core.util.path")
  if not ok then vim.notify("path.lua not available", vim.log.levels.ERROR); return end
  local root = path_util.find_root()
  if not root then vim.notify("Could not determine project root", vim.log.levels.WARN); return end
  pcall(function() vim.cmd.lcd(root); vim.notify("Project root: " .. root) end)
end, { desc = "LCD to project root (window-local)" })

cmd("CopyPath",    copy_path("%:p", "CopyPath"),   { desc = "Copy absolute file path" })
cmd("CopyRelPath", copy_path("%:.", "CopyRelPath"), { desc = "Copy relative file path" })

-- ── :BufOnly ──────────────────────────────────────────────────────────────────
cmd("BufOnly", function()
  local current = vim.api.nvim_get_current_buf()
  local ok, buffers = pcall(vim.api.nvim_list_bufs)
  if not ok then vim.notify("Failed to list buffers", vim.log.levels.ERROR); return end
  local deleted, skipped = 0, {}
  for _, buf in ipairs(buffers) do
    if buf ~= current and vim.api.nvim_buf_is_loaded(buf) then
      local del_ok = pcall(vim.api.nvim_buf_delete, buf, { force = false })
      if del_ok then deleted = deleted + 1
      else
        local name = vim.api.nvim_buf_get_name(buf)
        table.insert(skipped, name ~= "" and vim.fn.fnamemodify(name, ":t") or ("[buf " .. buf .. "]"))
      end
    end
  end
  local msg = string.format("Deleted %d buffer(s).", deleted)
  if #skipped > 0 then msg = msg .. " Skipped (modified): " .. table.concat(skipped, ", ") end
  vim.notify(msg)
end, { desc = "Delete all other buffers" })

cmd("CleanUp", function()
  pcall(function() collectgarbage("collect"); vim.notify(string.format("Memory: %.1fMB", collectgarbage("count") / 1024)) end)
end, { desc = "Run Lua garbage collection" })

cmd("ToggleWrap",  make_toggle("wrap",  "Wrap"),  { desc = "Toggle line wrap"   })
cmd("ToggleSpell", make_toggle("spell", "Spell"), { desc = "Toggle spell check" })

cmd("ToggleDiagnostics", function()
  local bufnr = vim.api.nvim_get_current_buf()
  local is_enabled
  local ok1 = pcall(function() is_enabled = vim.diagnostic.is_enabled({ bufnr = bufnr }) end)
  if not ok1 then
    local ok2 = pcall(function() is_enabled = vim.diagnostic.is_enabled() end)
    if not ok2 then vim.notify("Failed to check diagnostic status", vim.log.levels.ERROR); return end
  end
  pcall(function()
    if is_enabled then vim.diagnostic.enable(false, { bufnr = bufnr }); vim.notify("Diagnostics: disabled")
    else vim.diagnostic.enable(true, { bufnr = bufnr }); vim.notify("Diagnostics: enabled") end
  end)
end, { desc = "Toggle diagnostics (buffer-local)" })

cmd("ToggleAutoformat", function()
  pcall(function()
    vim.g.disable_autoformat = not vim.g.disable_autoformat
    vim.notify("Autoformat: " .. (vim.g.disable_autoformat and "disabled" or "enabled"))
  end)
end, { desc = "Toggle format-on-save" })

cmd("ToggleAutoCd", function()
  pcall(function()
    vim.g.auto_cd_root = not vim.g.auto_cd_root
    vim.notify("AutoCdRoot: " .. tostring(vim.g.auto_cd_root))
  end)
end, { desc = "Toggle auto-cd to project root" })

cmd("TelescopeResume", function()
  local ok, telescope = pcall(require, "telescope.builtin")
  if not ok then vim.notify("telescope not available", vim.log.levels.ERROR); return end
  pcall(telescope.resume)
end, { desc = "Resume last telescope picker" })

-- ── :MasonInstallAll ──────────────────────────────────────────────────────────
--
-- Supports an optional --dry-run flag:
--   :MasonInstallAll          — install everything not yet installed
--   :MasonInstallAll dry-run  — print what would be installed, do nothing
--
if vim.g._mason_install_running == nil then vim.g._mason_install_running = false end

cmd("MasonInstallAll", function(opts)
  local dry_run = opts.args and opts.args:find("dry%-run", 1, true) ~= nil

  local ok, registry = pcall(require, "mason-registry")
  if not ok then vim.notify("mason-registry not available", vim.log.levels.ERROR); return end

  if not dry_run and vim.g._mason_install_running then
    vim.notify("MasonInstallAll is already running — please wait", vim.log.levels.WARN); return
  end

  local pkgs = MASON_PACKAGES.get()   -- flat list via new M.get() API

  -- ── Dry-run mode ──────────────────────────────────────────────────────────
  if dry_run then
    local to_install, already = {}, {}
    for _, pkg_name in ipairs(pkgs) do
      local ok_pkg, pkg = pcall(function() return registry.get_package(pkg_name) end)
      if ok_pkg and pkg then
        if pkg:is_installed() then table.insert(already, pkg_name)
        else table.insert(to_install, pkg_name) end
      end
    end
    local lines = { "MasonInstallAll dry-run:" }
    if #to_install > 0 then
      table.insert(lines, "\nWould install (" .. #to_install .. "):")
      for _, n in ipairs(to_install) do table.insert(lines, "  • " .. n) end
    else
      table.insert(lines, "\nAll packages already installed.")
    end
    if #already > 0 then
      table.insert(lines, "\nAlready installed (" .. #already .. "):")
      for _, n in ipairs(already) do table.insert(lines, "  ✓ " .. n) end
    end
    vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO)
    return
  end

  -- ── Live install mode ─────────────────────────────────────────────────────
  vim.g._mason_install_running = true

  local TIMEOUT_MS = (type(vim.g.mason_install_timeout_ms) == "number" and vim.g.mason_install_timeout_ms > 0)
    and vim.g.mason_install_timeout_ms or 120000

  local total   = #pkgs
  local pending = 0
  local done    = 0
  local failed  = {}
  local timer   = nil

  -- Progress notification (replace-in-place via vim.notify id when available).
  local function progress_msg()
    return string.format("MasonInstallAll: %d / %d done%s",
      done, total, #failed > 0 and (" | failed: " .. #failed) or "")
  end

  local function finish()
    if timer then pcall(function() timer:stop(); timer:close() end); timer = nil end
    vim.g._mason_install_running = false
    if #failed > 0 then
      vim.notify(string.format("MasonInstallAll: %d/%d done. Failed: %s", done, total, table.concat(failed, ", ")), vim.log.levels.WARN)
    else
      vim.notify(string.format("MasonInstallAll: all %d packages installed.", done), vim.log.levels.INFO)
    end
  end

  local function check_done() if pending <= 0 then finish() end end

  timer = vim.uv.new_timer()
  timer:start(TIMEOUT_MS, 0, vim.schedule_wrap(function()
    vim.notify(string.format("MasonInstallAll: timed out after %ds (%d pending).\nIncrease: vim.g.mason_install_timeout_ms = %d",
      math.floor(TIMEOUT_MS / 1000), pending, TIMEOUT_MS * 2), vim.log.levels.WARN)
    finish()
  end))

  for _, pkg_name in ipairs(pkgs) do
    local ok_pkg, pkg = pcall(function() return registry.get_package(pkg_name) end)
    if not ok_pkg or not pkg then
      vim.notify("Not in registry: " .. pkg_name, vim.log.levels.WARN)
      table.insert(failed, pkg_name); done = done + 1
    elseif pkg:is_installed() then
      done = done + 1
    else
      pending = pending + 1
      vim.notify("Installing " .. pkg_name .. "… " .. progress_msg(), vim.log.levels.INFO)
      local ok_inst, installer = pcall(function() return pkg:install() end)
      if not ok_inst or type(installer) ~= "table" then
        pending = pending - 1; done = done + 1
        table.insert(failed, pkg_name)
        vim.notify(string.format("%s ✗ (install() failed: %s)", pkg_name, tostring(installer)), vim.log.levels.ERROR)
        check_done()
      else
        local ok_once = pcall(function()
          installer:once("closed", vim.schedule_wrap(function()
            pending = pending - 1; done = done + 1
            if pkg:is_installed() then
              vim.notify(pkg_name .. " ✓  " .. progress_msg(), vim.log.levels.INFO)
            else
              table.insert(failed, pkg_name)
              vim.notify(pkg_name .. " ✗  " .. progress_msg(), vim.log.levels.ERROR)
            end
            check_done()
          end))
        end)
        if not ok_once then
          pending = pending - 1; done = done + 1
          table.insert(failed, pkg_name)
          vim.notify(pkg_name .. " ✗ (listener failed)", vim.log.levels.ERROR)
          check_done()
        end
      end
    end
  end

  check_done()
end, { nargs = "?", desc = "Install all Mason packages (use 'dry-run' to preview)" })

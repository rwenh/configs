-- lua/core/commands.lua - Custom commands with comprehensive error handling
--
-- FIX (v2.3.11):
--   • "typescript-language-server" removed from MasonInstallAll. lsp.lua uses
--     typescript-tools.nvim (pmizio) for TS/JS — it registers its own internal
--     tsserver instance and never calls lspconfig.tsserver.setup(). Having
--     "typescript-language-server" in the list installed a Mason package that
--     was never wired into any server config, wasting disk space and causing
--     confusion during :checkhealth.
--   • "jdtls" added to MasonInstallAll LSP section. java.lua uses nvim-jdtls
--     directly (not mason-lspconfig), so mason-lspconfig's ensure_installed
--     never auto-installs it. MasonInstallAll is the only install path for jdtls
--     on a fresh setup.
--   • "sqls" added to MasonInstallAll LSP section. lsp.lua added sqls to
--     mason-lspconfig ensure_installed in v2.3.10, but MasonInstallAll still
--     lacked it — a manual :MasonInstall sqls was required on fresh setups that
--     ran MasonInstallAll before mason-lspconfig's auto_install triggered.
--
-- FIX (v2.3.15):
--   • ToggleAutoformat notification was inverted. The command first toggles
--     vim.g.disable_autoformat, then the old code read `not disable_autoformat`
--     to compute the display string — a double negation of a disable_* variable
--     that printed "false" when disabling and "true" when enabling.  Rewritten
--     to read the already-toggled value directly and map it to "enabled" /
--     "disabled" so the message is unambiguous.
--   • vim.g.disable_autoformat is now initialised to false in this file's
--     module-load preamble so the variable is always defined before the first
--     toggle call, consistent with auto_cd_root being initialised in options.lua.
--   • "gofumpt" added to MasonInstallAll formatters section. lsp.lua conform
--     wires go = { "goimports", "gofumpt" }; goimports was already in the list
--     but gofumpt was missing — a fresh :MasonInstallAll left the second Go
--     formatter uninstalled and format-on-save silently skipped the gofumpt step.

local cmd = vim.api.nvim_create_user_command

-- FIX (v2.3.15): initialise disable_autoformat so the variable exists before
-- the first ToggleAutoformat call. Mirrors the auto_cd_root = false pattern in
-- options.lua. This does not change default behaviour (nil and false are both
-- falsy in the format_on_save guard in lsp.lua).
if vim.g.disable_autoformat == nil then
  vim.g.disable_autoformat = false
end

-- ═══════════════════════════════════════════════════════════════════════════
-- HEALTH CHECK
-- ═══════════════════════════════════════════════════════════════════════════

cmd("Health", function()
  local ok_version, version = pcall(function() return vim.version() end)
  local ok_os, os_info       = pcall(function() return vim.uv.os_uname() end)
  local ok_clip              = pcall(function() return vim.fn.has("clipboard") == 1 end)
  local ok_lsp, lsp_clients  = pcall(function() return vim.lsp.get_clients() end)
  local ok_mem, mem          = pcall(function() return collectgarbage("count") / 1024 end)

  local health = {
    { "nvim",      ok_version and string.format("v%d.%d.%d",
        version.major, version.minor, version.patch) or "unknown" },
    { "os",        ok_os and os_info.sysname or "unknown" },
    { "clipboard", ok_clip and "✓" or "✗" },
    { "lsp",       ok_lsp and (#lsp_clients > 0
        and string.format("%d active", #lsp_clients) or "none") or "unknown" },
    { "memory",    ok_mem and string.format("%.1fMB", mem) or "unknown" },
    { "ide ver",   tostring(vim.g.nvim_ide_version or "unknown") },
  }

  print("=== Neovim Health ===")
  for _, pair in ipairs(health) do
    print(string.format("%-12s: %s", pair[1], pair[2]))
  end
end, { desc = "Show health status" })

-- ═══════════════════════════════════════════════════════════════════════════
-- FORMAT
-- FIX (v2.3.5): lsp_fallback = true → lsp_format = "fallback" (conform v6 API).
-- ═══════════════════════════════════════════════════════════════════════════

cmd("Format", function(opts)
  local ok, conform = pcall(require, "conform")
  if not ok then
    vim.notify("conform.nvim not available", vim.log.levels.ERROR)
    return
  end

  if opts.range > 0 then
    local end_line = opts.line2
    local end_col  = #vim.fn.getline(end_line)
    pcall(function()
      conform.format({
        lsp_format = "fallback",
        range = {
          start   = { opts.line1, 0 },
          ["end"] = { end_line, end_col },
        },
      })
    end)
  else
    pcall(function() conform.format({ lsp_format = "fallback" }) end)
  end
end, { range = true, desc = "Format file or range" })

-- ═══════════════════════════════════════════════════════════════════════════
-- PROJECT ROOT
-- ═══════════════════════════════════════════════════════════════════════════

cmd("ProjectRoot", function()
  local ok, path_util = pcall(require, "core.util.path")
  if not ok then
    vim.notify("path.lua not available", vim.log.levels.ERROR)
    return
  end

  local root = path_util.find_root()
  if not root then
    vim.notify("Could not determine project root", vim.log.levels.WARN)
    return
  end

  pcall(function()
    vim.cmd.cd(root)
    vim.notify("Project root: " .. root)
  end)
end, { desc = "Change to project root" })

-- ═══════════════════════════════════════════════════════════════════════════
-- COPY PATH
-- ═══════════════════════════════════════════════════════════════════════════

cmd("CopyPath", function()
  local path = vim.fn.expand("%:p")
  if path == "" then vim.notify("No file path available", vim.log.levels.WARN); return end
  pcall(function() vim.fn.setreg("+", path); vim.notify("Copied: " .. path) end)
end, { desc = "Copy file path" })

cmd("CopyRelPath", function()
  local path = vim.fn.expand("%:.")
  if path == "" then vim.notify("No file path available", vim.log.levels.WARN); return end
  pcall(function() vim.fn.setreg("+", path); vim.notify("Copied: " .. path) end)
end, { desc = "Copy relative path" })

-- ═══════════════════════════════════════════════════════════════════════════
-- DELETE OTHER BUFFERS
-- ═══════════════════════════════════════════════════════════════════════════

cmd("BufOnly", function()
  local current = vim.api.nvim_get_current_buf()
  local ok, buffers = pcall(function() return vim.api.nvim_list_bufs() end)
  if not ok then vim.notify("Failed to list buffers", vim.log.levels.ERROR); return end

  for _, buf in ipairs(buffers) do
    if buf ~= current and vim.api.nvim_buf_is_loaded(buf) then
      pcall(function() vim.api.nvim_buf_delete(buf, { force = false }) end)
    end
  end
  vim.notify("Deleted other buffers")
end, { desc = "Delete all other buffers" })

-- ═══════════════════════════════════════════════════════════════════════════
-- CLEAN UP MEMORY
-- ═══════════════════════════════════════════════════════════════════════════

cmd("CleanUp", function()
  pcall(function()
    collectgarbage("collect")
    local mem = collectgarbage("count") / 1024
    vim.notify(string.format("Memory: %.1fMB", mem))
  end)
end, { desc = "Run garbage collection" })

-- ═══════════════════════════════════════════════════════════════════════════
-- TOGGLE OPTIONS
-- ═══════════════════════════════════════════════════════════════════════════

cmd("ToggleWrap", function()
  pcall(function()
    vim.wo.wrap = not vim.wo.wrap
    vim.notify("Wrap: " .. tostring(vim.wo.wrap))
  end)
end, { desc = "Toggle line wrap" })

cmd("ToggleSpell", function()
  pcall(function()
    vim.wo.spell = not vim.wo.spell
    vim.notify("Spell: " .. tostring(vim.wo.spell))
  end)
end, { desc = "Toggle spell check" })

cmd("ToggleDiagnostics", function()
  local bufnr = vim.api.nvim_get_current_buf()
  local ok, is_enabled = pcall(function()
    return vim.diagnostic.is_enabled({ bufnr = bufnr })
  end)
  if not ok then
    local ok2, is_enabled2 = pcall(function() return vim.diagnostic.is_enabled() end)
    if not ok2 then
      vim.notify("Failed to check diagnostic status", vim.log.levels.ERROR)
      return
    end
    is_enabled = is_enabled2
  end
  pcall(function()
    if is_enabled then
      vim.diagnostic.enable(false, { bufnr = bufnr })
      vim.notify("Diagnostics disabled")
    else
      vim.diagnostic.enable(true, { bufnr = bufnr })
      vim.notify("Diagnostics enabled")
    end
  end)
end, { desc = "Toggle diagnostics (buffer-local)" })

-- FIX (v2.3.15): notification was doubly-negated. The old code toggled the
-- flag then called tostring(not disable_autoformat), meaning:
--   disable=true  (just disabled) → not true  = false → "Autoformat: false"
--   disable=false (just enabled)  → not false = true  → "Autoformat: true"
-- The message was technically correct but the double negation made "false"
-- mean "disabled", which is confusing. Now maps directly to "enabled" /
-- "disabled" for clarity, matching the ToggleDiagnostics message style.
cmd("ToggleAutoformat", function()
  pcall(function()
    vim.g.disable_autoformat = not vim.g.disable_autoformat
    local state = vim.g.disable_autoformat and "disabled" or "enabled"
    vim.notify("Autoformat: " .. state)
  end)
end, { desc = "Toggle autoformat" })

cmd("ToggleAutoCd", function()
  pcall(function()
    vim.g.auto_cd_root = not vim.g.auto_cd_root
    vim.notify("AutoCdRoot: " .. tostring(vim.g.auto_cd_root))
  end)
end, { desc = "Toggle auto cd to project root on BufEnter" })

-- ═══════════════════════════════════════════════════════════════════════════
-- TELESCOPE RESUME
-- ═══════════════════════════════════════════════════════════════════════════

cmd("TelescopeResume", function()
  local ok, telescope = pcall(require, "telescope.builtin")
  if not ok then vim.notify("telescope not available", vim.log.levels.ERROR); return end
  pcall(function() telescope.resume() end)
end, { desc = "Resume last telescope picker" })

-- ═══════════════════════════════════════════════════════════════════════════
-- INSTALL ALL MASON PACKAGES
-- ═══════════════════════════════════════════════════════════════════════════

cmd("MasonInstallAll", function()
  local ok, registry = pcall(require, "mason-registry")
  if not ok then vim.notify("mason-registry not available", vim.log.levels.ERROR); return end

  local pkgs = {
    -- LSP
    -- FIX (v2.3.9):  "fortls" added — lsp.lua attaches it as an optional server.
    -- FIX (v2.3.9):  "gopls" added — wired in lsp.lua servers table.
    -- FIX (v2.3.9b): "tailwindcss-language-server" added.
    -- FIX (v2.3.11): "typescript-language-server" REMOVED — lsp.lua uses
    --   typescript-tools.nvim which manages its own internal tsserver instance.
    --   lspconfig.tsserver is never configured anywhere in the project; this
    --   package was dead weight and caused :checkhealth confusion.
    -- FIX (v2.3.11): "jdtls" added — java.lua uses nvim-jdtls directly (not
    --   mason-lspconfig), so mason-lspconfig auto_install never pulls it.
    --   MasonInstallAll is the only install path on a fresh setup.
    -- FIX (v2.3.11): "sqls" added — lsp.lua added sqls to mason-lspconfig
    --   ensure_installed in v2.3.10 but MasonInstallAll still lacked it.
    "lua-language-server", "basedpyright",
    "html-lsp", "css-lsp", "json-lsp", "yaml-language-server",
    "clangd", "gopls", "solargraph", "elixir-ls", "kotlin-language-server",
    "tailwindcss-language-server",
    "zls", "fortls", "sqls", "jdtls",
    -- NOTE: cobol-language-server is NOT in the Mason registry.
    --       Install manually: npm i -g @broadcommfd/cobol-language-support
    -- NOTE: vhdl_ls (rust_hdl) must be installed via cargo:
    --       cargo install vhdl_ls  OR  :MasonInstall rust_hdl
    "rust_hdl",
    -- DAP
    "debugpy", "codelldb", "delve", "js-debug-adapter",
    "java-debug-adapter", "java-test",
    -- NOTE: elixir-ls includes the DAP debugger — no separate package needed.
    -- NOTE: kotlin-debug-adapter is NOT in Mason registry.
    --       Kotlin DAP uses java-debug-adapter via jdtls (already listed above).
    -- Formatters
    -- FIX (v2.3.15): "gofumpt" added. lsp.lua conform wires
    --   go = { "goimports", "gofumpt" } but only goimports was listed here,
    --   leaving the second Go formatter uninstalled on a fresh setup.
    "stylua", "prettier", "shfmt", "black", "isort",
    "goimports", "gofumpt", "ktlint", "rubocop", "clang-format", "fprettify",
    -- Linters
    "ruff", "eslint_d", "shellcheck", "htmlhint", "stylelint",
  }

  local total   = #pkgs
  local pending = 0
  local done    = 0
  local failed  = {}
  local timer   = nil

  local function check_done()
    if pending > 0 then return end
    if timer then timer:stop(); timer:close(); timer = nil end
    if #failed > 0 then
      vim.notify(
        string.format("MasonInstallAll: %d/%d done. Failed: %s",
          done, total, table.concat(failed, ", ")),
        vim.log.levels.WARN
      )
    else
      vim.notify(
        string.format("MasonInstallAll: all %d packages installed.", done),
        vim.log.levels.INFO
      )
    end
  end

  timer = vim.uv.new_timer()
  timer:start(60000, 0, vim.schedule_wrap(function()
    if pending > 0 then
      vim.notify(
        string.format("MasonInstallAll: timed out with %d installs still pending.", pending),
        vim.log.levels.WARN
      )
      if timer then timer:stop(); timer:close(); timer = nil end
    end
  end))

  for _, pkg_name in ipairs(pkgs) do
    local ok_pkg, pkg = pcall(function() return registry.get_package(pkg_name) end)

    if not ok_pkg or not pkg then
      vim.notify("Not in registry: " .. pkg_name, vim.log.levels.WARN)
      table.insert(failed, pkg_name)
      done = done + 1
    elseif pkg:is_installed() then
      vim.notify(pkg_name .. " already installed", vim.log.levels.INFO)
      done = done + 1
    else
      pending = pending + 1
      vim.notify("Installing " .. pkg_name .. "...", vim.log.levels.INFO)
      pcall(function()
        pkg:install():once("closed", vim.schedule_wrap(function()
          pending = pending - 1
          done    = done + 1
          if pkg:is_installed() then
            vim.notify(pkg_name .. " ✓", vim.log.levels.INFO)
          else
            table.insert(failed, pkg_name)
            vim.notify(pkg_name .. " ✗", vim.log.levels.ERROR)
          end
          check_done()
        end))
      end)
    end
  end

  check_done()
end, { desc = "Install all Mason packages" })

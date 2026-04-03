-- lua/core/commands.lua - Custom commands with comprehensive error handling

local cmd = vim.api.nvim_create_user_command

-- ════���══════════════════════════════════════════════════════════════════════
-- HEALTH CHECK
-- ═══════════════════════════════════════════════════════════════════════════

cmd("Health", function()
  -- FIX #11: Use an ordered list instead of pairs() for deterministic output.
  local ok_version, version = pcall(function()
    return vim.version()
  end)

  local ok_os, os_info = pcall(function()
    return vim.uv.os_uname()
  end)

  local ok_clip = pcall(function()
    return vim.fn.has("clipboard") == 1
  end)

  local ok_lsp, lsp_clients = pcall(function()
    return vim.lsp.get_clients()
  end)

  local ok_mem, mem = pcall(function()
    return collectgarbage("count") / 1024
  end)

  local health = {
    { "nvim", ok_version and string.format("v%d.%d.%d", version.major, version.minor, version.patch) or "unknown" },
    { "os", ok_os and os_info.sysname or "unknown" },
    { "clipboard", ok_clip and "✓" or "✗" },
    { "lsp", ok_lsp and (#lsp_clients > 0 and string.format("%d active", #lsp_clients) or "none") or "unknown" },
    { "memory", ok_mem and string.format("%.1fMB", mem) or "unknown" },
  }

  print("=== Neovim Health ===")
  for _, pair in ipairs(health) do
    print(string.format("%-12s: %s", pair[1], pair[2]))
  end
end, { desc = "Show health status" })

-- ═══════════════════════════════════════════════════════════════════════════
-- FORMAT
-- ═══════════════════════════════════════════════════════════════════════════

cmd("Format", function(opts)
  local ok, conform = pcall(require, "conform")
  if not ok then
    vim.notify("conform.nvim not available", vim.log.levels.ERROR)
    return
  end

  -- FIX #10a: Route through conform.nvim (not vim.lsp.buf.format directly)
  -- FIX #10b: End column calculation for proper range formatting
  if opts.range > 0 then
    local end_line   = opts.line2
    local end_col    = #vim.fn.getline(end_line)
    pcall(function()
      conform.format({
        lsp_fallback = true,
        range = {
          start  = { opts.line1, 0 },
          ["end"] = { end_line, end_col },
        },
      })
    end)
  else
    pcall(function()
      conform.format({ lsp_fallback = true })
    end)
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
  if path == "" then
    vim.notify("No file path available", vim.log.levels.WARN)
    return
  end
  pcall(function()
    vim.fn.setreg("+", path)
    vim.notify("Copied: " .. path)
  end)
end, { desc = "Copy file path" })

cmd("CopyRelPath", function()
  local path = vim.fn.expand("%:.")
  if path == "" then
    vim.notify("No file path available", vim.log.levels.WARN)
    return
  end
  pcall(function()
    vim.fn.setreg("+", path)
    vim.notify("Copied: " .. path)
  end)
end, { desc = "Copy relative path" })

-- ═══════════════════════════════════════════════════════════════════════════
-- DELETE OTHER BUFFERS
-- ═══════════════════════════════════════════════════════════════════════════

cmd("BufOnly", function()
  local current = vim.api.nvim_get_current_buf()
  local ok, buffers = pcall(function()
    return vim.api.nvim_list_bufs()
  end)

  if not ok then
    vim.notify("Failed to list buffers", vim.log.levels.ERROR)
    return
  end

  for _, buf in ipairs(buffers) do
    if buf ~= current and vim.api.nvim_buf_is_loaded(buf) then
      pcall(function()
        vim.api.nvim_buf_delete(buf, { force = false })
      end)
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

-- FIX #8: vim.diagnostic.is_enabled() with no args returns global state.
-- Added explicit comment on scope — enable(false) disables globally.
cmd("ToggleDiagnostics", function()
  local ok, is_enabled = pcall(function()
    return vim.diagnostic.is_enabled()
  end)

  if not ok then
    vim.notify("Failed to check diagnostic status", vim.log.levels.ERROR)
    return
  end

  pcall(function()
    if is_enabled then
      vim.diagnostic.enable(false)  -- disables globally across all buffers
      vim.notify("Diagnostics disabled")
    else
      vim.diagnostic.enable()       -- re-enables globally
      vim.notify("Diagnostics enabled")
    end
  end)
end, { desc = "Toggle diagnostics (global)" })

cmd("ToggleAutoformat", function()
  pcall(function()
    vim.g.disable_autoformat = not vim.g.disable_autoformat
    vim.notify("Autoformat: " .. tostring(not vim.g.disable_autoformat))
  end)
end, { desc = "Toggle autoformat" })

-- ═══════════════════════════════════════════════════════════════════════════
-- TELESCOPE RESUME
-- ═══════════════════════════════════════════════════════════════════════════

cmd("TelescopeResume", function()
  local ok, telescope = pcall(require, "telescope.builtin")
  if not ok then
    vim.notify("telescope not available", vim.log.levels.ERROR)
    return
  end
  pcall(function()
    telescope.resume()
  end)
end, { desc = "Resume last telescope picker" })

-- ═══════════════════════════════════════════════════════════════════════════
-- INSTALL ALL MASON PACKAGES
-- ═══════════════════════════════════════════════════════════════════════════

-- FIX #9: Rewrote completion counter for correct async tracking
cmd("MasonInstallAll", function()
  local ok, registry = pcall(require, "mason-registry")
  if not ok then
    vim.notify("mason-registry not available", vim.log.levels.ERROR)
    return
  end

  local pkgs = {
    -- LSP
    "lua-language-server",
    "basedpyright",
    "typescript-language-server",
    "html-lsp",
    "css-lsp",
    "json-lsp",
    "yaml-language-server",
    "clangd",
    "solargraph",
    "elixir-ls",
    "kotlin-language-server",
    "zls",
    "fortls",
    "sqls",
    "cobol-language-support",
    "rust_hdl",
    -- DAP
    "debugpy",
    "codelldb",
    "delve",
    "js-debug-adapter",
    "java-debug-adapter",
    "java-test",
    "elixir-ls-debugger",
    -- Formatters
    "stylua",
    "prettier",
    "shfmt",
    "black",
    "isort",
    "goimports",
    "ktlint",
    "rubocop",
    "clang-format",
    "fprettify",
    -- Linters
    "ruff",
    "eslint_d",
    "shellcheck",
    "htmlhint",
    "stylelint",
  }

  -- Separate tracking:
  -- pending   = async installs not yet resolved
  -- installed = successfully installed (async or already present)
  -- failed    = install failures + registry misses
  local pending   = 0
  local installed = 0
  local failed    = {}

  local function check_done()
    if pending > 0 then return end  -- still waiting on async installs
    if #failed > 0 then
      vim.notify("Done. Failed: " .. table.concat(failed, ", "), vim.log.levels.WARN)
    else
      vim.notify(
        string.format("All %d packages installed!", installed),
        vim.log.levels.INFO
      )
    end
  end

  for _, pkg_name in ipairs(pkgs) do
    local ok_pkg, pkg = pcall(function()
      return registry.get_package(pkg_name)
    end)

    if not ok_pkg or not pkg then
      vim.notify("Not in registry: " .. pkg_name, vim.log.levels.WARN)
      table.insert(failed, pkg_name)
    elseif pkg:is_installed() then
      installed = installed + 1
      vim.notify(pkg_name .. " already installed", vim.log.levels.INFO)
    else
      pending = pending + 1
      vim.notify("Installing " .. pkg_name .. "...", vim.log.levels.INFO)
      pcall(function()
        pkg:install():once("closed", vim.schedule_wrap(function()
          pending = pending - 1
          if pkg:is_installed() then
            installed = installed + 1
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

  -- If everything was already installed (no async work queued), fire now.
  check_done()
end, { desc = "Install all Mason packages" })

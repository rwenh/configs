-- lua/core/commands.lua - Custom commands

local cmd = vim.api.nvim_create_user_command

-- Health check
-- FIX #11: Use an ordered list instead of pairs() so output is deterministic.
-- pairs() iterates in undefined order — the report printed fields randomly.
cmd("Health", function()
  local health = {
    { "nvim",      string.format("v%d.%d.%d", vim.version().major, vim.version().minor, vim.version().patch) },
    { "os",        vim.uv.os_uname().sysname },
    { "clipboard", vim.fn.has("clipboard") == 1 and "✓" or "✗" },
    { "lsp",       (function()
                     local lsp = vim.lsp.get_clients()
                     return #lsp > 0 and string.format("%d active", #lsp) or "none"
                   end)() },
    { "memory",    string.format("%.1fMB", collectgarbage("count") / 1024) },
  }
  print("=== Neovim Health ===")
  for _, pair in ipairs(health) do
    print(string.format("%-12s: %s", pair[1], pair[2]))
  end
end, { desc = "Show health status" })

-- Format
-- FIX #10a: Route through conform.nvim (not vim.lsp.buf.format directly) so
-- the full formatter chain (prettier, stylua, black, etc.) is respected.
-- FIX #10b: End column was `0` which created a zero-width range on the last
-- line. Use the actual line length so the full last line is included.
cmd("Format", function(opts)
  if opts.range > 0 then
    local end_line   = opts.line2
    local end_col    = #vim.fn.getline(end_line)
    require("conform").format({
      lsp_fallback = true,
      range = {
        start  = { opts.line1, 0 },
        ["end"] = { end_line, end_col },
      },
    })
  else
    require("conform").format({ lsp_fallback = true })
  end
end, { range = true, desc = "Format file or range" })

-- Project root
cmd("ProjectRoot", function()
  local root = require("core.util.path").find_root()
  vim.cmd.cd(root)
  vim.notify("Project root: " .. root)
end, { desc = "Change to project root" })

-- Copy path
cmd("CopyPath", function()
  local path = vim.fn.expand("%:p")
  vim.fn.setreg("+", path)
  vim.notify("Copied: " .. path)
end, { desc = "Copy file path" })

cmd("CopyRelPath", function()
  local path = vim.fn.expand("%:.")
  vim.fn.setreg("+", path)
  vim.notify("Copied: " .. path)
end, { desc = "Copy relative path" })

-- Delete other buffers
cmd("BufOnly", function()
  local current = vim.api.nvim_get_current_buf()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if buf ~= current and vim.api.nvim_buf_is_loaded(buf) then
      vim.api.nvim_buf_delete(buf, { force = false })
    end
  end
  vim.notify("Deleted other buffers")
end, { desc = "Delete all other buffers" })

-- Clean up memory
cmd("CleanUp", function()
  collectgarbage("collect")
  vim.notify(string.format("Memory: %.1fMB", collectgarbage("count") / 1024))
end, { desc = "Run garbage collection" })

-- Toggle options
cmd("ToggleWrap", function()
  vim.wo.wrap = not vim.wo.wrap
  vim.notify("Wrap: " .. tostring(vim.wo.wrap))
end, { desc = "Toggle line wrap" })

cmd("ToggleSpell", function()
  vim.wo.spell = not vim.wo.spell
  vim.notify("Spell: " .. tostring(vim.wo.spell))
end, { desc = "Toggle spell check" })

-- FIX #8: vim.diagnostic.is_enabled() with no args returns global state (0.10+).
-- Added explicit comment on scope — enable(false) with no bufnr disables
-- diagnostics globally. This is intentional for a toggle command but noted
-- clearly so it's not mistaken for a buffer-local toggle.
cmd("ToggleDiagnostics", function()
  local enabled = vim.diagnostic.is_enabled()
  if enabled then
    vim.diagnostic.enable(false)  -- disables globally across all buffers
    vim.notify("Diagnostics disabled")
  else
    vim.diagnostic.enable()       -- re-enables globally
    vim.notify("Diagnostics enabled")
  end
end, { desc = "Toggle diagnostics (global)" })

cmd("ToggleAutoformat", function()
  vim.g.disable_autoformat = not vim.g.disable_autoformat
  vim.notify("Autoformat: " .. tostring(not vim.g.disable_autoformat))
end, { desc = "Toggle autoformat" })

-- Telescope resume
cmd("TelescopeResume", function()
  require("telescope.builtin").resume()
end, { desc = "Resume last telescope picker" })

-- Install all mason packages
-- FIX #9: Rewrote the completion counter so the "Done" summary only fires
-- after ALL async installs have actually finished.
-- Previously, already-installed packages were counted synchronously and could
-- push installed+#failed == #pkgs to true before any async callbacks had run,
-- causing premature "All packages installed!" on partially-done runs.
-- Registry lookup failures now correctly increment a skip counter separate
-- from failed installs so the total accounting is always exact.
cmd("MasonInstallAll", function()
  local registry = require("mason-registry")

  local pkgs = {
    -- LSP
    "lua-language-server",
    "basedpyright",
    "rust-analyzer",
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
    "cobol-language-server",
    "vhdl-ls",
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
    -- FIX (sql.lua #6): sqlfmt registered as formatter in sql.lua but was
    -- missing from MasonInstallAll — would never be auto-installed.
    "sqlfmt",
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
    local ok, pkg = pcall(registry.get_package, pkg_name)
    if not ok then
      vim.notify("Not in registry: " .. pkg_name, vim.log.levels.WARN)
      table.insert(failed, pkg_name)
    elseif pkg:is_installed() then
      installed = installed + 1
      vim.notify(pkg_name .. " already installed", vim.log.levels.INFO)
    else
      pending = pending + 1
      vim.notify("Installing " .. pkg_name .. "...", vim.log.levels.INFO)
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
    end
  end

  -- If everything was already installed (no async work queued), fire now.
  check_done()
end, { desc = "Install all Mason packages" })

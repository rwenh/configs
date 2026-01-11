-- lua/core/commands.lua - Custom commands

local cmd = vim.api.nvim_create_user_command

-- Health check
cmd("Health", function()
  local health = {}
  health.nvim = string.format("v%d.%d.%d", vim.version().major, vim.version().minor, vim.version().patch)
  health.os = vim.uv.os_uname().sysname
  health.clipboard = vim.fn.has("clipboard") == 1 and "✓" or "✗"
  local lsp = vim.lsp.get_clients()
  health.lsp = #lsp > 0 and string.format("%d active", #lsp) or "none"
  health.memory = string.format("%.1fMB", collectgarbage("count") / 1024)
  print("=== Neovim Health ===")
  for k, v in pairs(health) do
    print(string.format("%-12s: %s", k, v))
  end
end, { desc = "Show health status" })

-- Format
cmd("Format", function(opts)
  local range = opts.range > 0 and { start = { opts.line1, 0 }, ["end"] = { opts.line2, 0 } } or nil
  vim.lsp.buf.format({ range = range, timeout_ms = 3000 })
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
  local mem = collectgarbage("count") / 1024
  vim.notify(string.format("Memory: %.1fMB", mem))
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

cmd("ToggleDiagnostics", function()
  local current = vim.diagnostic.is_enabled()
  if current then
    vim.diagnostic.enable(false)
    vim.notify("Diagnostics disabled")
  else
    vim.diagnostic.enable()
    vim.notify("Diagnostics enabled")
  end
end, { desc = "Toggle diagnostics" })

cmd("ToggleAutoformat", function()
  vim.g.disable_autoformat = not vim.g.disable_autoformat
  vim.notify("Autoformat: " .. tostring(not vim.g.disable_autoformat))
end, { desc = "Toggle autoformat" })

-- Telescope resume
cmd("TelescopeResume", function()
  require("telescope.builtin").resume()
end, { desc = "Resume last telescope picker" })

-- Install all mason packages
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
    -- vhdl-ls must be installed via: cargo install vhdl_ls
    -- DAP
    "debugpy",
    "codelldb",
    "delve",
    "js-debug-adapter",
    -- Formatters
    "stylua",
    "prettier",
    "shfmt",
  }
  
  local installed = 0
  local total = #pkgs
  
  for _, pkg_name in ipairs(pkgs) do
    local ok, pkg = pcall(registry.get_package, pkg_name)
    if ok then
      if not pkg:is_installed() then
        vim.notify("Installing " .. pkg_name .. "...", vim.log.levels.INFO)
        pkg:install():once("closed", function()
          installed = installed + 1
          if installed == total then
            vim.notify("All packages installed!", vim.log.levels.INFO)
          end
        end)
      end
    else
      vim.notify("Package not found: " .. pkg_name, vim.log.levels.WARN)
    end
  end
end, { desc = "Install all Mason packages" })
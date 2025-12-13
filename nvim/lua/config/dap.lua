-- ~/.config/nvim/lua/config/dap.lua

local helpers = require("utils.helpers")

local dap = helpers.safe_require("dap")
if not dap then
  if _G.nvim_ide then
    _G.nvim_ide.dap_adapters = {}
  end
  return
end

local dapui = helpers.safe_require("dapui")
if not dapui then
  if _G.nvim_ide then
    _G.nvim_ide.dap_adapters = {}
  end
  return
end

local dap_virtual_text = helpers.safe_require("nvim-dap-virtual-text")

local dap_adapters = {}

local ok, err = pcall(function()
  dapui.setup({
    layouts = {
      { elements = { "scopes", "breakpoints", "stacks", "watches" }, size = 40, position = "left" },
      { elements = { "repl", "console" }, size = 0.25, position = "bottom" },
    },
    controls = {
      enabled = true,
      element = "repl",
    },
    floating = {
      border = "rounded",
      mappings = { close = { "q", "<Esc>" } },
    },
  })
end)

if not ok then
  vim.notify("DAP UI setup failed: " .. tostring(err), vim.log.levels.ERROR)
  return
end

if dap_virtual_text then
  pcall(function()
    dap_virtual_text.setup({
      enabled = true,
      commented = true,
    })
  end)
end

dap.listeners.after.event_initialized["dapui_config"] = function()
  pcall(dapui.open)
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  pcall(dapui.close)
end
dap.listeners.before.event_exited["dapui_config"] = function()
  pcall(dapui.close)
end

pcall(vim.fn.sign_define, 'DapBreakpoint', { text = '🔴', texthl = 'DapBreakpoint' })
pcall(vim.fn.sign_define, 'DapBreakpointCondition', { text = '🟡', texthl = 'DapBreakpoint' })
pcall(vim.fn.sign_define, 'DapBreakpointRejected', { text = '⚫', texthl = 'DapBreakpoint' })
pcall(vim.fn.sign_define, 'DapStopped', { text = '▶️', texthl = 'DapStopped', linehl = 'DapStoppedLine' })
pcall(vim.fn.sign_define, 'DapLogPoint', { text = '📝', texthl = 'DapLogPoint' })

local dap_python = helpers.safe_require("dap-python")
if dap_python then
  local mason_path = vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python"
  if vim.fn.filereadable(mason_path) == 1 then
    local success = pcall(dap_python.setup, mason_path)
    if success then dap_adapters.python = true end
  elseif helpers.command_exists("python3") then
    local success = pcall(dap_python.setup, "python3")
    if success then dap_adapters.python = true end
  end
end

local function setup_mason_adapter(pkg_name, setup_fn)
  local ok, mason_registry = pcall(require, "mason-registry")
  if not ok then return false end

  local has_pkg = pcall(function()
    return mason_registry.is_installed(pkg_name)
  end)

  if has_pkg then
    local get_ok, pkg = pcall(mason_registry.get_package, pkg_name)
    if get_ok then
      return pcall(setup_fn, pkg)
    end
  end
  return false
end

setup_mason_adapter("codelldb", function(pkg)
  local extension_path = pkg:get_install_path() .. "/extension/"
  local codelldb_path = extension_path .. "adapter/codelldb"
  local this_os = vim.uv.os_uname().sysname
  local liblldb_path = extension_path .. "lldb/lib/liblldb" ..
    (this_os == "Linux" and ".so" or this_os == "Darwin" and ".dylib" or ".dll")

  if vim.fn.filereadable(codelldb_path) == 1 then
    dap.adapters.codelldb = {
      type = "server",
      port = "${port}",
      executable = {
        command = codelldb_path,
        args = { "--port", "${port}" },
      }
    }

    dap.configurations.rust = {
      {
        name = "Launch",
        type = "codelldb",
        request = "launch",
        program = function()
          return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/target/debug/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
      },
    }
    dap_adapters.rust = true
  end
end)

setup_mason_adapter("node-debug2-adapter", function()
  dap.adapters.node2 = {
    type = "executable",
    command = "node",
    args = { vim.fn.stdpath("data") .. "/mason/packages/node-debug2-adapter/out/src/nodeDebug.js" },
  }

  dap.configurations.javascript = {
    {
      name = "Launch",
      type = "node2",
      request = "launch",
      program = "${file}",
      cwd = vim.fn.getcwd(),
      sourceMaps = true,
      protocol = "inspector",
      console = "integratedTerminal",
    },
  }

  dap.configurations.typescript = dap.configurations.javascript
  dap_adapters.javascript = true
  dap_adapters.typescript = true
end)

-- RECALIBRATED: Silent Go debugger setup - only if Go 1.25+ available
if helpers.command_exists("go") then
  local go_version_output = vim.fn.system("go version")
  local major, minor = go_version_output:match("go(%d+)%.(%d+)")

  if major and minor then
    major, minor = tonumber(major), tonumber(minor)

    if (major > 1) or (major == 1 and minor >= 25) then
      setup_mason_adapter("delve", function()
        dap.adapters.delve = {
          type = "server",
          port = "${port}",
          executable = {
            command = "dlv",
            args = { "dap", "-l", "127.0.0.1:${port}" },
          }
        }

        dap.configurations.go = {
          {
            type = "delve",
            name = "Debug",
            request = "launch",
            program = "${file}",
          },
          {
            type = "delve",
            name = "Debug test",
            request = "launch",
            mode = "test",
            program = "${file}",
          },
        }
        dap_adapters.go = true
      end)
    end
    -- REMOVED: Annoying notification about Go version
  end
end

vim.defer_fn(function()
  if _G.nvim_ide then
    _G.nvim_ide.dap_adapters = dap_adapters
  end
end, 100)

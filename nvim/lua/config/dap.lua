-- ~/.config/nvim/lua/config/dap.lua
-- Debug Adapter Protocol configuration

local helpers = require("utils.helpers")

local dap = helpers.safe_require("dap")
local dapui = helpers.safe_require("dapui")
local dap_virtual_text = helpers.safe_require("nvim-dap-virtual-text")

if not (dap and dapui) then return end

dapui.setup({
  layouts = {
    { elements = { "scopes", "breakpoints", "stacks" }, size = 40, position = "left" },
    { elements = { "repl", "console" }, size = 0.25, position = "bottom" },
  },
})

if dap_virtual_text then
  dap_virtual_text.setup({ enabled = true })
end

dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end

vim.fn.sign_define('DapBreakpoint', { text = '🔴', texthl = 'DapBreakpoint' })
vim.fn.sign_define('DapStopped', { text = '▶️', texthl = 'DapStopped' })

-- Python debugger
local dap_python = helpers.safe_require("dap-python")
if dap_python and helpers.command_exists("python3") then
  dap_python.setup("python3")
  _G.nvim_ide.dap_adapters.python = true
end

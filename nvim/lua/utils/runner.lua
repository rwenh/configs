-- ~/.config/nvim/lua/utils/runner.lua
-- Code execution system

local M = {}
local helpers = require("utils.helpers")

-- Code runners for different languages
local code_runners = {
  python = function(file)
    local interpreters = { "python3", "python", "py" }
    for _, interpreter in ipairs(interpreters) do
      if helpers.command_exists(interpreter) then
        return interpreter .. " " .. vim.fn.shellescape(file)
      end
    end
    return nil
  end,
  
  rust = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    if vim.fn.filereadable(helpers.join_path(dir, "Cargo.toml")) == 1 then
      return "cd " .. vim.fn.shellescape(dir) .. " && cargo run"
    else
      if not helpers.command_exists("rustc") then return nil end
      local exe = vim.fn.fnamemodify(file, ":r")
      return "rustc " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe) .. " && " .. vim.fn.shellescape(exe)
    end
  end,
  
  java = function(file)
    if not helpers.command_exists("javac") then return nil end
    local dir = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    return "cd " .. vim.fn.shellescape(dir) .. " && javac " .. vim.fn.shellescape(file) .. " && java " .. name
  end,
  
  javascript = function(file)
    if helpers.project_has_file("deno.json") or helpers.project_has_file("deno.jsonc") then
      if helpers.command_exists("deno") then
        return "deno run " .. vim.fn.shellescape(file)
      end
    end
    if helpers.command_exists("node") then
      return "node " .. vim.fn.shellescape(file)
    end
    return nil
  end,
  
  typescript = function(file)
    if helpers.project_has_file("deno.json") or helpers.project_has_file("deno.jsonc") then
      if helpers.command_exists("deno") then
        return "deno run " .. vim.fn.shellescape(file)
      end
    end
    if helpers.command_exists("tsx") then
      return "tsx " .. vim.fn.shellescape(file)
    elseif helpers.command_exists("ts-node") then
      return "ts-node " .. vim.fn.shellescape(file)
    end
    return nil
  end,
  
  lua = function(file)
    return "lua " .. vim.fn.shellescape(file)
  end,
  
  fortran = function(file)
    if not helpers.command_exists("gfortran") then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gfortran -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,
  
  c = function(file)
    if not helpers.command_exists("gcc") then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gcc " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe) .. " && " .. vim.fn.shellescape(exe)
  end,
  
  cpp = function(file)
    if not helpers.command_exists("g++") then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "g++ " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe) .. " && " .. vim.fn.shellescape(exe)
  end,
  
  sql = function(file)
    if helpers.command_exists("psql") then
      return "psql -f " .. vim.fn.shellescape(file)
    end
    return nil
  end,
}

-- Run current file
function M.run_file()
  local file = vim.fn.expand("%:p")
  local filetype = vim.bo.filetype
  
  if file == "" or vim.fn.filereadable(file) ~= 1 then
    vim.notify("No valid file to run", vim.log.levels.ERROR)
    return
  end
  
  if vim.bo.modified then
    vim.cmd("write")
  end
  
  local runner = code_runners[filetype]
  if not runner then
    vim.notify("No runner configured for " .. filetype, vim.log.levels.WARN)
    return
  end
  
  local cmd = runner(file)
  if not cmd then
    vim.notify("No suitable runtime found for " .. filetype, vim.log.levels.ERROR)
    return
  end
  
  local toggleterm_ok, Terminal = pcall(function()
    return require("toggleterm.terminal").Terminal
  end)
  
  if toggleterm_ok then
    local terminal = Terminal:new({
      cmd = cmd,
      direction = "float",
      close_on_exit = false,
      float_opts = { border = "curved" }
    })
    terminal:toggle()
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

-- Build project
function M.build_project()
  local build_cmd = nil
  
  if vim.fn.filereadable("Makefile") == 1 then
    build_cmd = "make"
  elseif vim.fn.filereadable("Cargo.toml") == 1 then
    build_cmd = "cargo build"
  elseif vim.fn.filereadable("build.gradle") == 1 then
    build_cmd = "./gradlew build"
  elseif vim.fn.filereadable("pom.xml") == 1 then
    build_cmd = "mvn package"
  else
    vim.notify("No known build file found", vim.log.levels.WARN)
    return
  end
  
  local toggleterm_ok, Terminal = pcall(function()
    return require("toggleterm.terminal").Terminal
  end)
  
  if toggleterm_ok then
    local terminal = Terminal:new({
      cmd = build_cmd,
      direction = "horizontal",
      close_on_exit = false,
    })
    terminal:toggle()
  else
    vim.cmd("split | terminal " .. build_cmd)
  end
end

return M

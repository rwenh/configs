-- lua/core/util/runner.lua - Code execution engine

local M = {}

local runners = {
  python = function(file)
    for _, cmd in ipairs({"python3", "python", "py"}) do
      if vim.fn.executable(cmd) == 1 then
        return cmd .. " " .. vim.fn.shellescape(file)
      end
    end
  end,
  rust = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    if vim.fn.filereadable(dir ..  "/Cargo.toml") == 1 then
      return "cd " .. vim.fn.shellescape(dir) .. " && cargo run"
    end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "rustc " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe) .. " && " .. vim.fn.shellescape(exe)
  end,
  go = function(file)
    return "go run " .. vim.fn.shellescape(file)
  end,
  javascript = function(file)
    if vim.fn.executable("node") == 1 then
      return "node " .. vim.fn.shellescape(file)
    end
  end,
  typescript = function(file)
    if vim.fn.executable("tsx") == 1 then
      return "tsx " .. vim.fn.shellescape(file)
    elseif vim.fn.executable("ts-node") == 1 then
      return "ts-node " .. vim.fn.shellescape(file)
    end
  end,
  lua = function(file)
    return "lua " .. vim.fn.shellescape(file)
  end,
  c = function(file)
    local exe = vim.fn. fnamemodify(file, ":r")
    return "gcc -Wall -o " .. vim. fn.shellescape(exe) .. " " .. vim.fn. shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,
  cpp = function(file)
    local exe = vim. fn.fnamemodify(file, ":r")
    return "g++ -Wall -std=c++17 -o " ..  vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,
  java = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn. fnamemodify(file, ":t:r")
    return "cd " .. vim.fn. shellescape(dir) .. " && javac " .. vim.fn.shellescape(file) .. " && java " .. name
  end,
  sh = function(file)
    return "bash " .. vim.fn.shellescape(file)
  end,
  bash = function(file)
    return "bash " .. vim. fn.shellescape(file)
  end,
}

function M.run_file()
  local file = vim.fn.expand("%:p")
  local ft = vim.bo.filetype

  if file == "" or vim. fn.filereadable(file) ~= 1 then
    vim.notify("No valid file to run", vim.log.levels. ERROR)
    return
  end

  if vim.bo.modified then
    vim.cmd("write")
  end

  local runner = runners[ft]
  if not runner then
    vim.notify("No runner for: " .. ft, vim.log.levels. WARN)
    return
  end

  local cmd = runner(file)
  if not cmd then
    vim.notify("No suitable runtime found", vim.log.levels. ERROR)
    return
  end

  local ok, term = pcall(require, "toggleterm. terminal")
  if ok then
    local Terminal = term.Terminal
    local t = Terminal:new({
      cmd = cmd,
      direction = "float",
      close_on_exit = false,
    })
    t:toggle()
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

function M.run_tests()
  local ft = vim.bo.filetype
  local test_commands = {
    python = "pytest",
    rust = "cargo test",
    go = "go test ./.. .",
    javascript = "npm test",
    typescript = "npm test",
  }

  local cmd = test_commands[ft]
  if not cmd then
    vim.notify("No test runner for: " .. ft, vim.log. levels.WARN)
    return
  end

  local ok, term = pcall(require, "toggleterm.terminal")
  if ok then
    local Terminal = term.Terminal
    local t = Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false })
    t:toggle()
  end
end

return M
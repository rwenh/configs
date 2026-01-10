-- lua/core/util/runner.lua - Code execution engine (updated with Ruby, Elixir, Kotlin, Zig, COBOL)

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
    if vim.fn.filereadable(dir .. "/Cargo.toml") == 1 then
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
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gcc -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,
  cpp = function(file)
    local exe = vim.fn.fnamemodify(file, ":r")
    return "g++ -Wall -std=c++17 -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,
  java = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    return "cd " .. vim.fn.shellescape(dir) .. " && javac " .. vim.fn.shellescape(file) .. " && java " .. name
  end,
  sh = function(file)
    return "bash " .. vim.fn.shellescape(file)
  end,
  bash = function(file)
    return "bash " .. vim.fn.shellescape(file)
  end,
  julia = function(file)
    if vim.fn.executable("julia") == 1 then
      return "julia " .. vim.fn.shellescape(file)
    end
  end,
  zig = function(file)
    if vim.fn.executable("zig") == 1 then
      return "zig run " .. vim.fn.shellescape(file)
    end
  end,
  nim = function(file)
    if vim.fn.executable("nim") == 1 then
      local exe = vim.fn.fnamemodify(file, ":r")
      return "nim c -r " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
    end
  end,
  -- New language runners
  ruby = function(file)
    if vim.fn.executable("ruby") == 1 then
      return "ruby " .. vim.fn.shellescape(file)
    end
  end,
  elixir = function(file)
    if vim.fn.executable("elixir") == 1 then
      return "elixir " .. vim.fn.shellescape(file)
    end
  end,
  kotlin = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    if vim.fn.executable("kotlinc") == 1 then
      return "cd " .. vim.fn.shellescape(dir) .. " && kotlinc " .. vim.fn.shellescape(file) .. " -include-runtime -d " .. name .. ".jar && java -jar " .. name .. ".jar"
    end
  end,
  cobol = function(file)
    if vim.fn.executable("cobc") == 1 then
      local exe = vim.fn.fnamemodify(file, ":r")
      return "cobc -x -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
    end
  end,
  vhdl = function(file)
    if vim.fn.executable("ghdl") == 1 then
      -- Extract entity name (basic heuristic - look for "entity X is")
      local entity = nil
      local f = io.open(file, "r")
      if f then
        for line in f:lines() do
          entity = line:match("entity%s+(%w+)%s+is")
          if entity then break end
        end
        f:close()
      end
      
      if entity then
        return string.format("ghdl -a %s && ghdl -e %s && ghdl -r %s", 
          vim.fn.shellescape(file), entity, entity)
      else
        return "ghdl -s " .. vim.fn.shellescape(file)
      end
    end
  end,
}

function M.run_file()
  local file = vim.fn.expand("%:p")
  local ft = vim.bo.filetype

  if file == "" or vim.fn.filereadable(file) ~= 1 then
    vim.notify("No valid file to run", vim.log.levels.ERROR)
    return
  end

  if vim.bo.modified then
    vim.cmd("write")
  end

  local runner = runners[ft]
  if not runner then
    vim.notify("No runner for: " .. ft, vim.log.levels.WARN)
    return
  end

  local cmd = runner(file)
  if not cmd then
    vim.notify("No suitable runtime found", vim.log.levels.ERROR)
    return
  end

  local ok, term = pcall(require, "toggleterm.terminal")
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

function M.run_selection()
  local ft = vim.bo.filetype
  local start_line = vim.fn.line("'<")
  local end_line = vim.fn.line("'>")
  local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)
  local code = table.concat(lines, "\n")

  local interpreters = {
    python = "python3",
    lua = "lua",
    javascript = "node",
    ruby = "ruby",
    elixir = "elixir",
  }

  local interpreter = interpreters[ft]
  if not interpreter then
    vim.notify("No interpreter for: " .. ft, vim.log.levels.WARN)
    return
  end

  local cmd = string.format("echo %s | %s", vim.fn.shellescape(code), interpreter)
  vim.cmd("split | terminal " .. cmd)
end

function M.run_tests()
  local ft = vim.bo.filetype
  local test_commands = {
    python = "pytest",
    rust = "cargo test",
    go = "go test ./...",
    javascript = "npm test",
    typescript = "npm test",
    ruby = "bundle exec rspec",
    elixir = "mix test",
  }

  local cmd = test_commands[ft]
  if not cmd then
    vim.notify("No test runner for: " .. ft, vim.log.levels.WARN)
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
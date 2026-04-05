-- lua/core/util/runner.lua - Code execution engine
--
-- FIX (v2.2.3):
--   • run_selection(): visual marks '<'/'>' are stale by the time an x-mode
--     map dispatches :lua. Fixed by reading nvim_buf_get_mark BEFORE the
--     mode switch via feedkeys, or by using line/col from the command range.
--     Solution: accept optional start/end line args (passed from the keymap
--     via a command range), fall back to marks only in normal mode.
--   • kotlin runner: jar name was not shellescape'd — spaces in project name
--     produced broken shell command.
--   • run_tests() kotlin/java: find_root() result used without shellescape
--     in the cd prefix — fixed throughout.

local M = {}

local runners = {
  python = function(file)
    for _, cmd in ipairs({ "python3", "python", "py" }) do
      if vim.fn.executable(cmd) == 1 then
        return cmd .. " " .. vim.fn.shellescape(file)
      end
    end
    return nil
  end,

  rust = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    if vim.fn.filereadable(dir .. "/Cargo.toml") == 1 then
      return "cd " .. vim.fn.shellescape(dir) .. " && cargo run"
    end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "rustc " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe)
      .. " && " .. vim.fn.shellescape(exe)
  end,

  go = function(file)
    return "go run " .. vim.fn.shellescape(file)
  end,

  javascript = function(file)
    if vim.fn.executable("node") == 1 then
      return "node " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  typescript = function(file)
    if vim.fn.executable("tsx") == 1 then
      return "tsx " .. vim.fn.shellescape(file)
    elseif vim.fn.executable("ts-node") == 1 then
      return "ts-node " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  lua = function(file)
    return "lua " .. vim.fn.shellescape(file)
  end,

  c = function(file)
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gcc -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
      .. " && " .. vim.fn.shellescape(exe)
  end,

  cpp = function(file)
    local exe = vim.fn.fnamemodify(file, ":r")
    return "g++ -Wall -std=c++17 -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
      .. " && " .. vim.fn.shellescape(exe)
  end,

  java = function(file)
    local dir  = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    return "cd " .. vim.fn.shellescape(dir)
      .. " && javac " .. vim.fn.shellescape(file)
      .. " && java " .. name
  end,

  sh   = function(file) return "bash " .. vim.fn.shellescape(file) end,
  bash = function(file) return "bash " .. vim.fn.shellescape(file) end,

  julia = function(file)
    if vim.fn.executable("julia") == 1 then
      return "julia " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  zig = function(file)
    if vim.fn.executable("zig") == 1 then
      return "zig run " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  nim = function(file)
    if vim.fn.executable("nim") == 1 then
      return "nim c -r " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  ruby = function(file)
    if vim.fn.executable("ruby") == 1 then
      return "ruby " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  elixir = function(file)
    if vim.fn.executable("elixir") == 1 then
      return "elixir " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  kotlin = function(file)
    local dir  = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    if vim.fn.executable("kotlinc") == 1 then
      -- FIX: shellescape the jar name — spaces in project name broke the cmd
      local jar = vim.fn.shellescape(name .. ".jar")
      return "cd " .. vim.fn.shellescape(dir)
        .. " && kotlinc " .. vim.fn.shellescape(file)
        .. " -include-runtime -d " .. jar
        .. " && java -jar " .. jar
    end
    return nil
  end,

  cobol = function(file)
    if vim.fn.executable("cobc") == 1 then
      local exe = vim.fn.fnamemodify(file, ":r")
      return "cobc -x -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
        .. " && " .. vim.fn.shellescape(exe)
    end
    return nil
  end,

  vhdl = function(file)
    if vim.fn.executable("ghdl") == 1 then
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
          vim.fn.shellescape(file),
          vim.fn.shellescape(entity),
          vim.fn.shellescape(entity))
      else
        return "ghdl -s " .. vim.fn.shellescape(file)
      end
    end
    return nil
  end,

  fortran = function(file)
    if vim.fn.executable("gfortran") == 1 then
      local exe = vim.fn.fnamemodify(file, ":r")
      return "gfortran -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
        .. " && " .. vim.fn.shellescape(exe)
    end
    return nil
  end,
}

local selection_interpreters = {
  python     = function() return vim.fn.executable("python3") == 1 and "python3" or "python" end,
  lua        = function() return "lua" end,
  javascript = function() return vim.fn.executable("node") == 1 and "node" or nil end,
  typescript = function()
    if vim.fn.executable("tsx") == 1 then return "tsx" end
    if vim.fn.executable("ts-node") == 1 then return "ts-node" end
    return nil
  end,
  ruby   = function() return vim.fn.executable("ruby") == 1 and "ruby" or nil end,
  elixir = function() return vim.fn.executable("elixir") == 1 and "elixir" or nil end,
  sh     = function() return "bash" end,
  bash   = function() return "bash" end,
  julia  = function() return vim.fn.executable("julia") == 1 and "julia" or nil end,
}

function M.run_file()
  local file = vim.fn.expand("%:p")
  local ft   = vim.bo.filetype

  if file == "" or vim.fn.filereadable(file) ~= 1 then
    vim.notify("No valid file to run", vim.log.levels.ERROR)
    return
  end

  if vim.bo.modified and vim.bo.buftype == "" then
    local ok = pcall(function() vim.cmd("write") end)
    if not ok then
      vim.notify("Failed to save buffer before running", vim.log.levels.WARN)
    end
  end

  local runner = runners[ft]
  if not runner then
    vim.notify("No runner for: " .. ft, vim.log.levels.WARN)
    return
  end

  local cmd = runner(file)
  if not cmd then
    vim.notify("No suitable runtime found for: " .. ft, vim.log.levels.ERROR)
    return
  end

  local ok, term = pcall(require, "toggleterm.terminal")
  if ok and term then
    pcall(function()
      term.Terminal:new({
        cmd           = cmd,
        direction     = "float",
        close_on_exit = false,
      }):toggle()
    end)
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

-- FIX: run_selection() — visual marks '<'/'>' are stale after mode exit.
-- The keymap must pass the line range explicitly:
--   map("x", "<leader>'s", ":<C-u>lua require('core.util.runner').run_selection(vim.fn.line(\"'<\"), vim.fn.line(\"'>\"))<CR>")
-- The args are evaluated WHILE still in visual mode (via :<C-u> which exits
-- visual but keeps the range), so marks are still valid at call time.
-- Fallback (no args) reads marks and warns if they are 0.
function M.run_selection(start_line, end_line)
  local ft  = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

  -- Prefer explicit line args (passed from keymap before mode switch)
  if not start_line or not end_line then
    local ok_s, mark_s = pcall(vim.api.nvim_buf_get_mark, buf, "<")
    local ok_e, mark_e = pcall(vim.api.nvim_buf_get_mark, buf, ">")
    if not ok_s or not ok_e then
      vim.notify("Could not read visual selection marks", vim.log.levels.ERROR)
      return
    end
    start_line = mark_s[1]
    end_line   = mark_e[1]
  end

  if start_line == 0 or end_line == 0 then
    vim.notify("No visual selection found — make a selection first", vim.log.levels.WARN)
    return
  end

  local lines = vim.api.nvim_buf_get_lines(buf, start_line - 1, end_line, false)
  local code  = table.concat(lines, "\n")

  local resolver = selection_interpreters[ft]
  if not resolver then
    vim.notify("No selection interpreter for: " .. ft, vim.log.levels.WARN)
    return
  end

  local interpreter = resolver()
  if not interpreter then
    vim.notify("Runtime not found for: " .. ft, vim.log.levels.ERROR)
    return
  end

  local tmpfile = vim.fn.tempname() .. "." .. ft
  local f = io.open(tmpfile, "w")
  if not f then
    vim.notify("Failed to create temp file for selection run", vim.log.levels.ERROR)
    return
  end

  local ok_write = pcall(function()
    f:write(code)
    f:close()
  end)

  if not ok_write then
    vim.notify("Failed to write to temp file", vim.log.levels.ERROR)
    pcall(os.remove, tmpfile)
    return
  end

  local cmd = interpreter .. " " .. vim.fn.shellescape(tmpfile)

  local function cleanup()
    vim.defer_fn(function() pcall(os.remove, tmpfile) end, 500)
  end

  local ok_t, term = pcall(require, "toggleterm.terminal")
  if ok_t and term then
    pcall(function()
      local t = term.Terminal:new({
        cmd           = cmd,
        direction     = "float",
        close_on_exit = false,
        on_exit       = function() cleanup() end,
      })
      t:toggle()
    end)
  else
    vim.api.nvim_create_autocmd("TermClose", {
      once     = true,
      callback = function() cleanup() end,
    })
    vim.cmd("split | terminal " .. cmd)
  end
end

function M.run_tests()
  local ft = vim.bo.filetype

  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()

  -- FIX: shellescape(root) in all cd-prefixed commands
  local escaped_root = vim.fn.shellescape(root)

  local test_commands = {
    python     = "pytest",
    rust       = "cargo test",
    go         = "go test ./...",
    javascript = "npm test",
    typescript = "npm test",
    ruby       = "bundle exec rspec",
    elixir     = "mix test",
    kotlin = vim.fn.filereadable(root .. "/gradlew") == 1
      and "cd " .. escaped_root .. " && ./gradlew test"
      or  "cd " .. escaped_root .. " && mvn test",
    java = vim.fn.filereadable(root .. "/gradlew") == 1
      and "cd " .. escaped_root .. " && ./gradlew test"
      or  "cd " .. escaped_root .. " && mvn test",
    zig = "zig build test",
  }

  local cmd = test_commands[ft]
  if not cmd then
    vim.notify("No test runner for: " .. ft, vim.log.levels.WARN)
    return
  end

  local ok, term = pcall(require, "toggleterm.terminal")
  if ok and term then
    pcall(function()
      term.Terminal:new({
        cmd           = cmd,
        direction     = "float",
        close_on_exit = false,
      }):toggle()
    end)
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

return M

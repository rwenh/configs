-- lua/core/util/runner.lua - Code execution engine
--
-- OPT (v2.3.14):
--   • run_file(), run_tests(), run_selection() now all delegate terminal
--     launch to core.util.term.float() / float_at_root(). The three
--     independent inline toggleterm boilerplate blocks that existed here
--     have been removed. term.lua was introduced specifically for this
--     purpose; runner.lua was the last caller that hadn't adopted it.
--   • detect_js_test_cmd() remains a public export (promoted in v2.3.11).

local M = {}

-- ── Internal helpers ──────────────────────────────────────────────────────

-- Lazy-require term so we don't hard-depend at module load time.
local function term()
  local ok, t = pcall(require, "core.util.term")
  if not ok then
    vim.notify("[runner] core.util.term not available — falling back to split terminal",
      vim.log.levels.WARN)
    return nil
  end
  return t
end

-- Walk up from dir, return first ancestor containing marker file (up to N levels).
local function find_ancestor_with(dir, marker, levels)
  levels = levels or 5
  local current = dir
  for _ = 1, levels do
    if vim.fn.filereadable(current .. "/" .. marker) == 1
    or vim.fn.isdirectory(current .. "/" .. marker) == 1 then
      return current
    end
    local parent = vim.fn.fnamemodify(current, ":h")
    if parent == current then break end
    current = parent
  end
  return nil
end

-- ── File runners ──────────────────────────────────────────────────────────

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
    local cargo_root = find_ancestor_with(dir, "Cargo.toml", 5)
    if cargo_root then
      return "cd " .. vim.fn.shellescape(cargo_root) .. " && cargo run"
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

-- ── Selection interpreters ────────────────────────────────────────────────

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

-- ── Public: run_file ──────────────────────────────────────────────────────

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

  -- OPT (v2.3.14): delegate to term.float(); fallback to split terminal.
  local t = term()
  if t then
    t.float(cmd)
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

-- ── Public: run_selection ─────────────────────────────────────────────────

function M.run_selection(start_line, end_line)
  local ft  = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

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

  -- OPT (v2.3.14): delegate to term.float() with on_exit cleanup.
  local t = term()
  if t then
    t.float(cmd, { on_exit = function() cleanup() end })
  else
    vim.api.nvim_create_autocmd("TermClose", {
      once     = true,
      callback = function() cleanup() end,
    })
    vim.cmd("split | terminal " .. cmd)
  end
end

-- ── Public: detect_js_test_cmd ────────────────────────────────────────────
-- Promoted to public export in v2.3.11 so test.lua can reuse it.
-- Both callers must pass the project root.
function M.detect_js_test_cmd(root)
  if vim.fn.filereadable(root .. "/bun.lockb") == 1
  or vim.fn.filereadable(root .. "/bun.lock")  == 1 then
    return "bun test"
  elseif vim.fn.filereadable(root .. "/pnpm-lock.yaml") == 1 then
    return "pnpm test"
  elseif vim.fn.filereadable(root .. "/yarn.lock") == 1 then
    return "yarn test"
  end
  return "npm test"
end

-- ── Public: run_tests ─────────────────────────────────────────────────────

function M.run_tests()
  local ft = vim.bo.filetype

  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()

  local escaped_root = vim.fn.shellescape(root)

  local test_commands = {
    python     = "cd " .. escaped_root .. " && pytest",
    rust       = "cd " .. escaped_root .. " && cargo test",
    go         = "cd " .. escaped_root .. " && go test ./...",
    javascript = "cd " .. escaped_root .. " && " .. M.detect_js_test_cmd(root),
    typescript = "cd " .. escaped_root .. " && " .. M.detect_js_test_cmd(root),
    ruby       = "cd " .. escaped_root .. " && bundle exec rspec",
    elixir     = "cd " .. escaped_root .. " && mix test",
    kotlin = vim.fn.filereadable(root .. "/gradlew") == 1
      and "cd " .. escaped_root .. " && ./gradlew test"
      or  "cd " .. escaped_root .. " && mvn test",
    java = vim.fn.filereadable(root .. "/gradlew") == 1
      and "cd " .. escaped_root .. " && ./gradlew test"
      or  "cd " .. escaped_root .. " && mvn test",
    zig        = "cd " .. escaped_root .. " && zig build test",
    c   = vim.fn.executable("ctest") == 1
      and "cd " .. escaped_root .. " && ctest --test-dir build --output-on-failure"
      or  nil,
    cpp = vim.fn.executable("ctest") == 1
      and "cd " .. escaped_root .. " && ctest --test-dir build --output-on-failure"
      or  nil,
  }

  -- Informational messages for languages with no generic test runner.
  local no_test_runner_info = {
    fortran = "Fortran has no standard unit-test runner.\n"
      .. "Use <leader>ftb to build & run, or <leader>ftm for make.",
    vhdl    = "VHDL has no standard unit-test runner.\n"
      .. "Use <leader>vhr (GHDL Run & View) or <leader>vha/<vhe> to simulate.",
    cobol   = "COBOL has no standard unit-test runner.\n"
      .. "Use <leader>cob to compile & run the current file.",
    c   = vim.fn.executable("ctest") == 0
      and "C/C++ tests use CTest but `ctest` was not found.\n"
        .. "Install CMake (which ships ctest) and build the project first: <leader>ccb."
      or  nil,
    cpp = vim.fn.executable("ctest") == 0
      and "C/C++ tests use CTest but `ctest` was not found.\n"
        .. "Install CMake (which ships ctest) and build the project first: <leader>ccb."
      or  nil,
  }

  local info_msg = no_test_runner_info[ft]
  if info_msg then
    vim.notify(info_msg, vim.log.levels.INFO)
    return
  end

  local cmd = test_commands[ft]
  if not cmd then
    vim.notify("No test runner for: " .. ft, vim.log.levels.WARN)
    return
  end

  -- OPT (v2.3.14): delegate to term.float(); fallback to split terminal.
  local t = term()
  if t then
    t.float(cmd)
  else
    vim.cmd("split | terminal " .. cmd)
  end
end

return M

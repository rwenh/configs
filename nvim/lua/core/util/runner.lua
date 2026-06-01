-- lua/core/util/runner.lua — code execution engine
--

local M = {}

-- ── Lazy-cached module handles ─────────────────────────────────────────────────
local _term = nil
local function term()
  if _term then return _term end
  local ok, t = pcall(require, "core.util.term")
  if ok then _term = t; return _term end
  vim.notify("[runner] core.util.term not available — falling back to split terminal",
    vim.log.levels.WARN)
  return nil
end

local _path = nil
local function path()
  if _path then return _path end
  local ok, p = pcall(require, "core.util.path")
  if ok then _path = p; return _path end
  return nil
end

---@param root string   project root directory
---@param task string   gradle/maven task name (e.g. "test", "build")
---@return string|nil
function M.gradle_or_maven(root, task)
  local escaped = vim.fn.shellescape(root)
  local gradlew  = root .. "/gradlew"
  if vim.fn.filereadable(gradlew) == 1 and vim.fn.executable(gradlew) == 1 then
    return "cd " .. escaped .. " && ./gradlew " .. task
  end
  if vim.fn.filereadable(root .. "/pom.xml") == 1 then
    local mvn_task = (task == "run") and "exec:java" or task
    return "cd " .. escaped .. " && mvn " .. mvn_task
  end
  return nil
end

-- ── File runners ───────────────────────────────────────────────────────────────

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
    local dir        = vim.fn.fnamemodify(file, ":h")
    local p          = path()
    local cargo_root = p and p.find_root(dir)
    if cargo_root and vim.fn.filereadable(cargo_root .. "/Cargo.toml") == 1 then
      return "cd " .. vim.fn.shellescape(cargo_root) .. " && cargo run"
    end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "rustc " .. vim.fn.shellescape(file)
      .. " -o " .. vim.fn.shellescape(exe)
      .. " && "  .. vim.fn.shellescape(exe)
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

  javascriptreact = function(file)
    if vim.fn.executable("tsx") == 1 then
      return "tsx " .. vim.fn.shellescape(file)
    end
    return vim.fn.executable("node") == 1
      and "node " .. vim.fn.shellescape(file) or nil
  end,

  typescriptreact = function(file)
    if vim.fn.executable("tsx") == 1 then
      return "tsx " .. vim.fn.shellescape(file)
    elseif vim.fn.executable("ts-node") == 1 then
      return "ts-node " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  lua = function(file)
    if vim.fn.executable("lua") ~= 1 then return nil end
    return "lua " .. vim.fn.shellescape(file)
  end,

  c = function(file)
    if vim.fn.executable("gcc") ~= 1 then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gcc -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
      .. " && " .. vim.fn.shellescape(exe)
  end,

  cpp = function(file)
    if vim.fn.executable("g++") ~= 1 then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "g++ -Wall -std=c++17 -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
      .. " && " .. vim.fn.shellescape(exe)
  end,

  java = function(file)
    if vim.fn.executable("javac") ~= 1 or vim.fn.executable("java") ~= 1 then return nil end
    local dir  = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    return "cd " .. vim.fn.shellescape(dir)
      .. " && javac " .. vim.fn.shellescape(file)
      .. " && java " .. name
  end,

  sh   = function(file) return "bash " .. vim.fn.shellescape(file) end,
  bash = function(file) return "bash " .. vim.fn.shellescape(file) end,

  julia = function(file)
    return vim.fn.executable("julia") == 1
      and "julia " .. vim.fn.shellescape(file) or nil
  end,

  zig = function(file)
    return vim.fn.executable("zig") == 1
      and "zig run " .. vim.fn.shellescape(file) or nil
  end,

  nim = function(file)
    return vim.fn.executable("nim") == 1
      and "nim c -r " .. vim.fn.shellescape(file) or nil
  end,

  ruby = function(file)
    return vim.fn.executable("ruby") == 1
      and "ruby " .. vim.fn.shellescape(file) or nil
  end,

  elixir = function(file)
    return vim.fn.executable("elixir") == 1
      and "elixir " .. vim.fn.shellescape(file) or nil
  end,

  kotlin = function(file)
    if vim.fn.executable("kotlinc") ~= 1 then return nil end
    local dir  = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    local jar  = vim.fn.shellescape(name .. ".jar")
    return "cd " .. vim.fn.shellescape(dir)
      .. " && kotlinc " .. vim.fn.shellescape(file)
      .. " -include-runtime -d " .. jar
      .. " && java -jar " .. jar
  end,

  cobol = function(file)
    if vim.fn.executable("cobc") ~= 1 then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "cobc -x -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
      .. " && " .. vim.fn.shellescape(exe)
  end,

  -- ── VHDL runner ──────────────────────────────────────────────────────────────
  vhdl = function(file)
    if vim.fn.executable("ghdl") ~= 1 then return nil end

    local entity = nil
    local ok_rf, lines = pcall(vim.fn.readfile, file)
    if ok_rf and lines then
      for _, line in ipairs(lines) do
        entity = line:match("entity%s+(%w+)%s+is")
        if entity then break end
      end
    end

    if entity then
      local vcd = "/tmp/nvim_ghdl_wave.vcd"
      local cmd = string.format(
        "ghdl -a %s && ghdl -e %s && ghdl -r %s --vcd=%s",
        vim.fn.shellescape(file),
        vim.fn.shellescape(entity),
        vim.fn.shellescape(entity),
        vim.fn.shellescape(vcd)
      )
      if vim.fn.executable("gtkwave") == 1 then
        cmd = cmd .. " && gtkwave " .. vim.fn.shellescape(vcd)
      end
      return cmd
    end

    vim.notify(
      "[runner] VHDL: no 'entity … is' declaration found in\n"
      .. vim.fn.fnamemodify(file, ":t") .. "\n"
      .. "Falling back to syntax check only (ghdl -s).\n"
      .. "Ensure the entity declaration uses the standard form:\n"
      .. "  entity <name> is\n"
      .. "Or use <leader>vha to analyze and <leader>vhe to elaborate manually.",
      vim.log.levels.WARN
    )
    return "ghdl -s " .. vim.fn.shellescape(file)
  end,

  fortran = function(file)
    if vim.fn.executable("gfortran") ~= 1 then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gfortran -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file)
      .. " && " .. vim.fn.shellescape(exe)
  end,
}

-- ── Selection interpreters ─────────────────────────────────────────────────────

local selection_interpreters = {
  python = function()
    return vim.fn.executable("python3") == 1 and "python3" or "python"
  end,
  lua = function()
    for _, v in ipairs({ "lua", "lua5.4", "lua5.3", "lua5.2", "lua5.1" }) do
      if vim.fn.executable(v) == 1 then return v end
    end
    return nil
  end,
  javascript = function()
    return vim.fn.executable("node") == 1 and "node" or nil
  end,
  typescript = function()
    if vim.fn.executable("tsx")     == 1 then return "tsx"     end
    if vim.fn.executable("ts-node") == 1 then return "ts-node" end
    return nil
  end,
  javascriptreact = function()
    if vim.fn.executable("tsx")  == 1 then return "tsx"  end
    return vim.fn.executable("node") == 1 and "node" or nil
  end,
  typescriptreact = function()
    if vim.fn.executable("tsx")     == 1 then return "tsx"     end
    if vim.fn.executable("ts-node") == 1 then return "ts-node" end
    return nil
  end,
  ruby   = function() return vim.fn.executable("ruby")   == 1 and "ruby"   or nil end,
  elixir = function() return vim.fn.executable("elixir") == 1 and "elixir" or nil end,
  sh     = function() return "bash" end,
  bash   = function() return "bash" end,
  julia  = function() return vim.fn.executable("julia")  == 1 and "julia"  or nil end,
}

-- ── Public: run_file ───────────────────────────────────────────────────────────

function M.run_file()
  if vim.bo.buftype ~= "" then
    vim.notify("[runner] run_file: not a normal file buffer", vim.log.levels.WARN)
    return
  end

  local file = vim.fn.expand("%:p")
  local ft   = vim.bo.filetype

  if file == "" or vim.fn.filereadable(file) ~= 1 then
    vim.notify("[runner] no valid file to run", vim.log.levels.ERROR)
    return
  end

  if vim.bo.modified and (vim.g.runner_autosave ~= false) then
    local ok = pcall(vim.cmd, "write")
    if not ok then
      vim.notify("[runner] failed to save buffer before running", vim.log.levels.WARN)
    end
  end

  local runner_fn = runners[ft]
  if not runner_fn then
    vim.notify("[runner] no runner for filetype: " .. ft, vim.log.levels.WARN)
    return
  end

  local cmd = runner_fn(file)
  if not cmd then
    vim.notify("[runner] no suitable runtime found for: " .. ft, vim.log.levels.ERROR)
    return
  end

  local t = term()
  if t then t.float(cmd) else vim.cmd("split | terminal " .. cmd) end
end

-- ── Public: run_selection ──────────────────────────────────────────────────────

function M.run_selection(start_line, end_line)
  local ft  = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

  -- ── Resolve visual marks when caller does not supply explicit lines ─────────
  if not start_line or not end_line then
    local ok_s, mark_s = pcall(vim.api.nvim_buf_get_mark, buf, "<")
    local ok_e, mark_e = pcall(vim.api.nvim_buf_get_mark, buf, ">")

    local lcount   = vim.api.nvim_buf_line_count(buf)
    local sl       = ok_s and mark_s[1] or 0
    local el       = ok_e and mark_e[1] or 0
    local marks_ok = ok_s and ok_e
      and sl > 0 and el > 0
      and sl <= lcount and el <= lcount

    if not marks_ok then
      if not ok_s or not ok_e then
        vim.notify("[runner] could not read visual selection marks", vim.log.levels.ERROR)
      elseif sl == 0 or el == 0 then
        vim.notify("[runner] no visual selection — make a selection first",
          vim.log.levels.WARN)
      else
        vim.notify(
          "[runner] visual marks are stale (buffer changed since last selection).\n"
          .. "Re-select the lines and try again.",
          vim.log.levels.WARN
        )
      end
      return
    end

    start_line = sl
    end_line   = el
  end

  if start_line > end_line then
    start_line, end_line = end_line, start_line
  end

  local lines = vim.api.nvim_buf_get_lines(buf, start_line - 1, end_line, false)
  local code  = table.concat(lines, "\n")

  local resolver = selection_interpreters[ft]
  if not resolver then
    vim.notify("[runner] no selection interpreter for: " .. ft, vim.log.levels.WARN)
    return
  end

  local interpreter = resolver()
  if not interpreter then
    vim.notify("[runner] runtime not found for: " .. ft, vim.log.levels.ERROR)
    return
  end

  local EXT_MAP = {
    javascript       = "js",
    typescript       = "ts",
    javascriptreact  = "js",
    typescriptreact  = "ts",
    sh               = "sh",
    bash             = "sh",
    lua              = "lua",
  }
  local ext     = EXT_MAP[ft] or ft
  local tmpfile = vim.fn.tempname() .. "." .. ext

  local write_ok = pcall(vim.fn.writefile, vim.split(code, "\n"), tmpfile)
  if not write_ok then
    vim.notify("[runner] failed to write temp file for selection run",
      vim.log.levels.ERROR)
    return
  end

  local cmd = interpreter .. " " .. vim.fn.shellescape(tmpfile)

  -- ── Cleanup helpers ─────────────────────────────────────────────────────────
  local cleaned = false
  local function cleanup()
    if cleaned then return end
    cleaned = true
    pcall(os.remove, tmpfile)
  end

  local t = term()
  if t then
    t.float(cmd, {
      on_exit = function() vim.schedule(cleanup) end,
    })
  else
    -- Fallback: plain split terminal.
    local aug_name = "RunnerSelectionCleanup_" .. vim.fn.sha256(tmpfile):sub(1, 8)
    vim.api.nvim_create_autocmd("TermClose", {
      once     = true,
      group    = vim.api.nvim_create_augroup(aug_name, { clear = true }),
      callback = function()
        vim.schedule(cleanup)
        pcall(vim.api.nvim_del_augroup_by_name, aug_name)
      end,
    })
    vim.defer_fn(cleanup, 2000)
    vim.cmd("split | terminal " .. cmd)
  end
end

-- ── Public: detect_js_test_cmd ────────────────────────────────────────────────

---@param root string  project root directory
---@return string      bare package-manager test command (no cd prefix)
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

-- ── Public: run_tests ─────────────────────────────────────────────────────────

function M.run_tests()
  local ft = vim.bo.filetype

  local ok_path, path = pcall(require, "core.util.path")
  local root          = (ok_path and path.find_root()) or vim.fn.getcwd()
  local er            = vim.fn.shellescape(root)

  local info_msgs = {
    fortran = "Fortran has no standard unit-test runner.\n"
      .. "Use <leader>ftb to build & run, or <leader>ftm for make.",
    vhdl    = "VHDL has no standard unit-test runner.\n"
      .. "Use <leader>vhr (GHDL Run & View) or <leader>vha/<vhe> to simulate.",
    cobol   = "COBOL has no standard unit-test runner.\n"
      .. "Use <leader>cob to compile & run the current file.",
  }
  if info_msgs[ft] then
    vim.notify(info_msgs[ft], vim.log.levels.INFO)
    return
  end

  local function ctest_thunk()
    if vim.fn.executable("ctest") ~= 1 then
      vim.notify(
        "C/C++ tests use CTest but `ctest` was not found.\n"
          .. "Install CMake (which ships ctest) and build first: <leader>ccb.",
        vim.log.levels.INFO
      )
      return nil
    end
    return "cd " .. er .. " && ctest --test-dir build --output-on-failure"
  end

  local js_test = function() return "cd " .. er .. " && " .. M.detect_js_test_cmd(root) end

  local dispatch = {
    python          = function() return "cd " .. er .. " && pytest" end,
    rust            = function() return "cd " .. er .. " && cargo test" end,
    go              = function() return "cd " .. er .. " && go test ./..." end,
    javascript      = js_test,
    typescript      = js_test,
    javascriptreact = js_test,
    typescriptreact = js_test,
    ruby            = function() return "cd " .. er .. " && bundle exec rspec" end,
    elixir          = function() return "cd " .. er .. " && mix test" end,
    kotlin          = function() return M.gradle_or_maven(root, "test") end,
    java            = function() return M.gradle_or_maven(root, "test") end,
    zig             = function() return "cd " .. er .. " && zig build test" end,
    c               = ctest_thunk,
    cpp             = ctest_thunk,
  }

  local thunk = dispatch[ft]
  if not thunk then
    vim.notify("[runner] no test runner for: " .. ft, vim.log.levels.WARN)
    return
  end

  local cmd = thunk()
  if not cmd then return end

  local t = term()
  if t then t.float(cmd) else vim.cmd("split | terminal " .. cmd) end
end

return M

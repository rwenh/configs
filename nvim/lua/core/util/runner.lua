-- lua/core/util/runner.lua — code execution engine
--

local M = {}

local _term = nil
local function term()
  if _term then return _term end
  local ok, t = pcall(require, "core.util.term")
  if ok then _term = t; return _term end
  vim.notify("[runner] core.util.term not available", vim.log.levels.WARN)
  return nil
end

local _path = nil
local function path()
  if _path then return _path end
  local ok, p = pcall(require, "core.util.path")
  if ok then _path = p; return _path end
  return nil
end

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
      if vim.fn.executable(cmd) == 1 then return cmd .. " " .. vim.fn.shellescape(file) end
    end
  end,
  rust = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    local p   = path()
    local cargo_root = p and p.find_root(dir)
    if cargo_root and vim.fn.filereadable(cargo_root .. "/Cargo.toml") == 1 then
      return "cd " .. vim.fn.shellescape(cargo_root) .. " && cargo run"
    end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "rustc " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe) .. " && " .. vim.fn.shellescape(exe)
  end,
  go              = function(file) return "go run " .. vim.fn.shellescape(file) end,
  javascript      = function(file) return vim.fn.executable("node") == 1 and "node " .. vim.fn.shellescape(file) or nil end,
  typescript      = function(file)
    if vim.fn.executable("tsx")     == 1 then return "tsx "     .. vim.fn.shellescape(file) end
    if vim.fn.executable("ts-node") == 1 then return "ts-node " .. vim.fn.shellescape(file) end
  end,
  javascriptreact = function(file)
    if vim.fn.executable("tsx")  == 1 then return "tsx "  .. vim.fn.shellescape(file) end
    return vim.fn.executable("node") == 1 and "node " .. vim.fn.shellescape(file) or nil
  end,
  typescriptreact = function(file)
    if vim.fn.executable("tsx")     == 1 then return "tsx "     .. vim.fn.shellescape(file) end
    if vim.fn.executable("ts-node") == 1 then return "ts-node " .. vim.fn.shellescape(file) end
  end,
  lua    = function(file) return vim.fn.executable("lua") == 1 and "lua " .. vim.fn.shellescape(file) or nil end,
  c      = function(file) if vim.fn.executable("gcc") ~= 1 then return nil end; local exe = vim.fn.fnamemodify(file, ":r"); return "gcc -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe) end,
  cpp    = function(file) if vim.fn.executable("g++") ~= 1 then return nil end; local exe = vim.fn.fnamemodify(file, ":r"); return "g++ -Wall -std=c++17 -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe) end,
  java   = function(file) if vim.fn.executable("javac") ~= 1 or vim.fn.executable("java") ~= 1 then return nil end; local dir = vim.fn.fnamemodify(file, ":h"); local name = vim.fn.fnamemodify(file, ":t:r"); return "cd " .. vim.fn.shellescape(dir) .. " && javac " .. vim.fn.shellescape(file) .. " && java " .. name end,
  sh     = function(file) return "bash " .. vim.fn.shellescape(file) end,
  bash   = function(file) return "bash " .. vim.fn.shellescape(file) end,
  julia  = function(file) return vim.fn.executable("julia")  == 1 and "julia "  .. vim.fn.shellescape(file) or nil end,
  zig    = function(file) return vim.fn.executable("zig")    == 1 and "zig run " .. vim.fn.shellescape(file) or nil end,
  nim    = function(file) return vim.fn.executable("nim")    == 1 and "nim c -r " .. vim.fn.shellescape(file) or nil end,
  ruby   = function(file) return vim.fn.executable("ruby")   == 1 and "ruby "   .. vim.fn.shellescape(file) or nil end,
  elixir = function(file) return vim.fn.executable("elixir") == 1 and "elixir " .. vim.fn.shellescape(file) or nil end,
  kotlin = function(file)
    if vim.fn.executable("kotlinc") ~= 1 then return nil end
    local dir = vim.fn.fnamemodify(file, ":h"); local name = vim.fn.fnamemodify(file, ":t:r")
    local jar = vim.fn.shellescape(name .. ".jar")
    return "cd " .. vim.fn.shellescape(dir) .. " && kotlinc " .. vim.fn.shellescape(file) .. " -include-runtime -d " .. jar .. " && java -jar " .. jar
  end,
  cobol   = function(file) if vim.fn.executable("cobc") ~= 1 then return nil end; local exe = vim.fn.fnamemodify(file, ":r"); return "cobc -x -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe) end,
  fortran = function(file) if vim.fn.executable("gfortran") ~= 1 then return nil end; local exe = vim.fn.fnamemodify(file, ":r"); return "gfortran -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe) end,
  vhdl    = function(file)
    if vim.fn.executable("ghdl") ~= 1 then return nil end
    local entity = nil
    local ok_rf, lines = pcall(vim.fn.readfile, file)
    if ok_rf and lines then
      for _, line in ipairs(lines) do entity = line:match("entity%s+(%w+)%s+is"); if entity then break end end
    end
    if entity then
      local vcd = "/tmp/nvim_ghdl_wave.vcd"
      local cmd = string.format("ghdl -a %s && ghdl -e %s && ghdl -r %s --vcd=%s", vim.fn.shellescape(file), vim.fn.shellescape(entity), vim.fn.shellescape(entity), vim.fn.shellescape(vcd))
      if vim.fn.executable("gtkwave") == 1 then cmd = cmd .. " && gtkwave " .. vim.fn.shellescape(vcd) end
      return cmd
    end
    return "ghdl -s " .. vim.fn.shellescape(file)
  end,
}

-- ── Selection interpreters ─────────────────────────────────────────────────────
local selection_interpreters = {
  python          = function() return vim.fn.executable("python3") == 1 and "python3" or "python" end,
  lua             = function() for _, v in ipairs({"lua","lua5.4","lua5.3","lua5.2","lua5.1"}) do if vim.fn.executable(v)==1 then return v end end end,
  javascript      = function() return vim.fn.executable("node") == 1 and "node" or nil end,
  typescript      = function() if vim.fn.executable("tsx")==1 then return "tsx" end; if vim.fn.executable("ts-node")==1 then return "ts-node" end end,
  javascriptreact = function() if vim.fn.executable("tsx")==1 then return "tsx" end; return vim.fn.executable("node")==1 and "node" or nil end,
  typescriptreact = function() if vim.fn.executable("tsx")==1 then return "tsx" end; if vim.fn.executable("ts-node")==1 then return "ts-node" end end,
  ruby            = function() return vim.fn.executable("ruby")   == 1 and "ruby"   or nil end,
  elixir          = function() return vim.fn.executable("elixir") == 1 and "elixir" or nil end,
  sh              = function() return "bash" end,
  bash            = function() return "bash" end,
  julia           = function() return vim.fn.executable("julia")  == 1 and "julia"  or nil end,
}

-- ── Public: run_file ───────────────────────────────────────────────────────────
function M.run_file()
  if vim.bo.buftype ~= "" then vim.notify("[runner] run_file: not a normal file buffer", vim.log.levels.WARN); return end
  local file = vim.fn.expand("%:p")
  local ft   = vim.bo.filetype
  if file == "" or vim.fn.filereadable(file) ~= 1 then vim.notify("[runner] no valid file to run", vim.log.levels.ERROR); return end
  if vim.bo.modified and (vim.g.runner_autosave ~= false) then pcall(vim.cmd, "write") end
  local runner_fn = runners[ft]
  if not runner_fn then vim.notify("[runner] no runner for filetype: " .. ft, vim.log.levels.WARN); return end
  local cmd = runner_fn(file)
  if not cmd then vim.notify("[runner] no suitable runtime found for: " .. ft, vim.log.levels.ERROR); return end
  local t = term()
  if t then t.float(cmd) else vim.cmd("split | terminal " .. cmd) end
end

-- ── Public: run_nearest_function ──────────────────────────────────────────────
--
-- Currently supported: Python (pytest -k), JavaScript/
-- TypeScript (vitest/jest --testNamePattern), Go (GoTestFunc), Rust (cargo test).
-- Falls back to run_file for unsupported filetypes.
--
function M.run_nearest_function()
  local ft  = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

  -- Try to extract the function name via Treesitter.
  local function_name = nil
  pcall(function()
    local ts_ok, ts = pcall(require, "nvim-treesitter.ts_utils")
    if not ts_ok then return end
    local node = ts.get_node_at_cursor()
    while node do
      local node_type = node:type()
      if node_type == "function_definition" or node_type == "method_definition"
      or node_type == "function_declaration" or node_type == "method_declaration"
      or node_type == "arrow_function" then
        -- Get the name child node.
        for i = 0, node:named_child_count() - 1 do
          local child = node:named_child(i)
          if child:type() == "identifier" or child:type() == "name" then
            local start_row, start_col, end_row, end_col = child:range()
            function_name = vim.api.nvim_buf_get_text(buf, start_row, start_col, end_row, end_col, {})[1]
            break
          end
        end
        if function_name then break end
      end
      node = node:parent()
    end
  end)

  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  local er   = vim.fn.shellescape(root)

  local dispatch = {
    python = function()
      if not function_name then return "cd " .. er .. " && pytest" end
      return "cd " .. er .. " && pytest -k " .. vim.fn.shellescape(function_name) .. " -v"
    end,
    javascript = function()
      if not function_name then return "cd " .. er .. " && npm test" end
      return "cd " .. er .. " && npx vitest run -t " .. vim.fn.shellescape(function_name)
    end,
    typescript = function()
      if not function_name then return "cd " .. er .. " && npm test" end
      return "cd " .. er .. " && npx vitest run -t " .. vim.fn.shellescape(function_name)
    end,
    go = function()
      if not function_name then return "cd " .. er .. " && go test ./..." end
      return "cd " .. er .. " && go test ./... -run " .. vim.fn.shellescape("^" .. function_name .. "$")
    end,
    rust = function()
      if not function_name then return "cd " .. er .. " && cargo test" end
      return "cd " .. er .. " && cargo test " .. vim.fn.shellescape(function_name)
    end,
  }

  local thunk = dispatch[ft]
  if not thunk then
    vim.notify(
      "[runner] run_nearest_function: unsupported filetype '" .. ft .. "' — falling back to run_file",
      vim.log.levels.INFO
    )
    M.run_file()
    return
  end

  if function_name then
    vim.notify("[runner] Running nearest function: " .. function_name, vim.log.levels.INFO)
  end

  local cmd = thunk()
  if not cmd then return end
  local t = term()
  if t then t.float(cmd) else vim.cmd("split | terminal " .. cmd) end
end

-- ── Public: run_selection ──────────────────────────────────────────────────────
function M.run_selection(start_line, end_line)
  local ft  = vim.bo.filetype
  local buf = vim.api.nvim_get_current_buf()

  if not start_line or not end_line then
    local ok_s, mark_s = pcall(vim.api.nvim_buf_get_mark, buf, "<")
    local ok_e, mark_e = pcall(vim.api.nvim_buf_get_mark, buf, ">")
    local lcount = vim.api.nvim_buf_line_count(buf)
    local sl = ok_s and mark_s[1] or 0
    local el = ok_e and mark_e[1] or 0
    if not (ok_s and ok_e and sl > 0 and el > 0 and sl <= lcount and el <= lcount) then
      vim.notify("[runner] no valid visual selection", vim.log.levels.WARN); return
    end
    start_line = sl; end_line = el
  end
  if start_line > end_line then start_line, end_line = end_line, start_line end

  local lines = vim.api.nvim_buf_get_lines(buf, start_line - 1, end_line, false)
  local code  = table.concat(lines, "\n")
  local resolver = selection_interpreters[ft]
  if not resolver then vim.notify("[runner] no selection interpreter for: " .. ft, vim.log.levels.WARN); return end
  local interpreter = resolver()
  if not interpreter then vim.notify("[runner] runtime not found for: " .. ft, vim.log.levels.ERROR); return end

  local EXT_MAP = { javascript="js", typescript="ts", javascriptreact="js", typescriptreact="ts", sh="sh", bash="sh", lua="lua" }
  local tmpfile = vim.fn.tempname() .. "." .. (EXT_MAP[ft] or ft)
  if not pcall(vim.fn.writefile, vim.split(code, "\n"), tmpfile) then
    vim.notify("[runner] failed to write temp file", vim.log.levels.ERROR); return
  end

  local cmd     = interpreter .. " " .. vim.fn.shellescape(tmpfile)
  local cleaned = false
  local function cleanup() if not cleaned then cleaned = true; pcall(os.remove, tmpfile) end end
  local t = term()
  if t then t.float(cmd, { on_exit = function() vim.schedule(cleanup) end })
  else
    local aug = "RunnerSelectionCleanup_" .. vim.fn.sha256(tmpfile):sub(1, 8)
    vim.api.nvim_create_autocmd("TermClose", {
      once = true, group = vim.api.nvim_create_augroup(aug, { clear = true }),
      callback = function() vim.schedule(cleanup); pcall(vim.api.nvim_del_augroup_by_name, aug) end,
    })
    vim.defer_fn(cleanup, 2000)
    vim.cmd("split | terminal " .. cmd)
  end
end

-- ── Public: pipe ──────────────────────────────────────────────────────────────
--
-- Example:
--   runner.pipe("jq .", '{"name":"test"}')
--   runner.pipe("prettier --parser typescript", buffer_content)
--
---@param cmd        string   shell command that reads stdin and writes stdout
---@param stdin_text string   text to pipe into the command
---@param opts       table?   { title = string? }
function M.pipe(cmd, stdin_text, opts)
  if type(cmd) ~= "string" or cmd == "" then
    vim.notify("[runner] pipe(): cmd must be non-empty", vim.log.levels.ERROR); return
  end
  opts = opts or {}

  local tmpfile = vim.fn.tempname()
  if not pcall(vim.fn.writefile, vim.split(stdin_text or "", "\n"), tmpfile) then
    vim.notify("[runner] pipe(): failed to write stdin temp file", vim.log.levels.ERROR); return
  end

  local full_cmd = "cat " .. vim.fn.shellescape(tmpfile) .. " | " .. cmd
  local t = term()
  if t then
    t.float(full_cmd, {
      on_exit = function() vim.schedule(function() pcall(os.remove, tmpfile) end) end,
    })
  else
    vim.cmd("split | terminal " .. full_cmd)
    vim.defer_fn(function() pcall(os.remove, tmpfile) end, 3000)
  end
end

-- ── Public: detect_js_test_cmd ────────────────────────────────────────────────
function M.detect_js_test_cmd(root)
  if vim.fn.filereadable(root .. "/bun.lockb") == 1 or vim.fn.filereadable(root .. "/bun.lock") == 1 then return "bun test"
  elseif vim.fn.filereadable(root .. "/pnpm-lock.yaml") == 1 then return "pnpm test"
  elseif vim.fn.filereadable(root .. "/yarn.lock") == 1 then return "yarn test"
  end
  return "npm test"
end

-- ── Public: run_tests ─────────────────────────────────────────────────────────
function M.run_tests()
  local ft = vim.bo.filetype
  local ok_path, path_mod = pcall(require, "core.util.path")
  local root = (ok_path and path_mod.find_root()) or vim.fn.getcwd()
  local er   = vim.fn.shellescape(root)

  local info_msgs = {
    fortran = "Fortran has no standard unit-test runner.\nUse <leader>ftb to build & run.",
    vhdl    = "VHDL has no standard unit-test runner.\nUse <leader>vhr (GHDL Run & View).",
    cobol   = "COBOL has no standard unit-test runner.\nUse <leader>cob to compile & run.",
  }
  if info_msgs[ft] then vim.notify(info_msgs[ft], vim.log.levels.INFO); return end

  local function ctest_thunk()
    if vim.fn.executable("ctest") ~= 1 then
      vim.notify("C/C++ tests use CTest but `ctest` was not found.", vim.log.levels.INFO); return nil
    end
    return "cd " .. er .. " && ctest --test-dir build --output-on-failure"
  end

  local js_test = function() return "cd " .. er .. " && " .. M.detect_js_test_cmd(root) end
  local dispatch = {
    python          = function() return "cd " .. er .. " && pytest" end,
    rust            = function() return "cd " .. er .. " && cargo test" end,
    go              = function() return "cd " .. er .. " && go test ./..." end,
    javascript      = js_test, typescript = js_test,
    javascriptreact = js_test, typescriptreact = js_test,
    ruby            = function() return "cd " .. er .. " && bundle exec rspec" end,
    elixir          = function() return "cd " .. er .. " && mix test" end,
    kotlin          = function() return M.gradle_or_maven(root, "test") end,
    java            = function() return M.gradle_or_maven(root, "test") end,
    zig             = function() return "cd " .. er .. " && zig build test" end,
    c = ctest_thunk, cpp = ctest_thunk,
  }

  local thunk = dispatch[ft]
  if not thunk then vim.notify("[runner] no test runner for: " .. ft, vim.log.levels.WARN); return end
  local cmd = thunk()
  if not cmd then return end
  local t = term()
  if t then t.float(cmd) else vim.cmd("split | terminal " .. cmd) end
end

return M

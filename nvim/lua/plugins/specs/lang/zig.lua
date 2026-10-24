-- lua/plugins/specs/lang/zig.lua — Zig language support
--

local shared = require("plugins.specs.lang.shared")
local exec   = require("core.util.exec")

local function path() return require("core.util.path") end
local function term() return require("core.util.term") end

local function notify(msg, level)
  vim.notify("[zig] " .. msg, level or vim.log.levels.INFO)
end

-- ── build.zig location ────────────────────────────────────────────────────────
---@return string|nil  directory holding build.zig, nearest to the current buffer
local function build_root()
  return path().find_up({ "build.zig" })
end

---@param file string
---@return string[]  lines of the file, or {} when it cannot be read
local function read_lines(file)
  local ok, lines = pcall(vim.fn.readfile, file)
  return ok and lines or {}
end

-- ── build.zig parsing ─────────────────────────────────────────────────────────

---@param lines string[]
---@return string  the source with line comments removed, joined by newlines
local function strip_line_comments(lines)
  local out = {}
  for _, line in ipairs(lines) do
    local i, in_str, cut = 1, false, nil
    while i <= #line do
      local c = line:sub(i, i)
      if in_str then
        if c == "\\" then
          i = i + 1
        elseif c == '"' then
          in_str = false
        end
      elseif c == "'" and line:sub(i + 1, i + 1) == '"' and line:sub(i + 2, i + 2) == "'" then
        i = i + 2
      elseif c == '"' then
        in_str = true
      elseif c == "/" and line:sub(i + 1, i + 1) == "/" then
        cut = i
        break
      end
      i = i + 1
    end
    out[#out + 1] = cut and line:sub(1, cut - 1) or line
  end
  return table.concat(out, "\n")
end

---@return string|nil  nil when the parentheses do not balance
local function call_args(text, open)
  local depth, i, n, in_str = 0, open, #text, false
  while i <= n do
    local c = text:sub(i, i)
    if in_str then
      if c == "\\" then
        i = i + 1
      elseif c == '"' then
        in_str = false
      end
    elseif c == '"' then
      in_str = true
    elseif c == "(" then
      depth = depth + 1
    elseif c == ")" then
      depth = depth - 1
      if depth == 0 then return text:sub(open + 1, i - 1) end
    end
    i = i + 1
  end
  return nil
end

---@param lines string[]
---@return string[]
local function exe_names(lines)
  local text = strip_line_comments(lines)
  local names, seen = {}, {}
  local function add(name)
    if name and not seen[name] then
      seen[name] = true
      names[#names + 1] = name
    end
  end

  local pos = 1
  while true do
    local _, open = text:find("addExecutable%s*%(", pos)
    if not open then break end
    local args = call_args(text, open)
    if args then
      if args:match("^%s*%.%s*{") then
        add(args:match('%.name%s*=%s*"([^"]+)"'))
      else
        add(args:match('^%s*"([^"]+)"'))
      end
    end
    pos = open + 1
  end
  return names
end

-- ── Built binaries ────────────────────────────────────────────────────────────
---@param p string
---@return boolean
local function is_executable_file(p)
  return vim.fn.isdirectory(p) == 0 and vim.fn.executable(p) == 1
end

---@param bin_dir string
---@param names string[]
---@return string[]
local function built_executables(bin_dir, names)
  local found = {}
  for _, n in ipairs(names) do
    local p = bin_dir .. "/" .. n
    if is_executable_file(p) then found[#found + 1] = p end
  end
  if #found == 0 then
    for _, p in ipairs(vim.fn.glob(bin_dir .. "/*", false, true)) do
      if is_executable_file(p) then found[#found + 1] = p end
    end
  end
  return found
end

---@return string|table
local function pick_debuggee()
  local root = build_root() or vim.fn.getcwd()
  local bin_dir = root .. "/zig-out/bin"
  local names = exe_names(read_lines(root .. "/build.zig"))
  local found = built_executables(bin_dir, names)
  if #found == 1 then return found[1] end

  local default = found[1]
    or (names[1] and (bin_dir .. "/" .. names[1]))
    or (bin_dir .. "/")
  local choice = vim.fn.input("Path to exe: ", default, "file")
  if choice == "" then return require("dap").ABORT end
  return choice
end

-- ── DAP configuration ─────────────────────────────────────────────────────────
vim.api.nvim_create_autocmd("FileType", {
  pattern  = "zig",
  group    = vim.api.nvim_create_augroup("ZigDap", { clear = true }),
  callback = function()
    local ok, dap = pcall(require, "dap")
    if not ok or dap.configurations.zig ~= nil then return end
    dap.configurations.zig = {
      {
        name        = "Launch Zig binary",
        type        = "codelldb",
        request     = "launch",
        program     = pick_debuggee,
        cwd         = "${workspaceFolder}",
        stopOnEntry = false,
      },
    }
  end,
  desc = "Register the Zig debug configuration (codelldb) when a Zig buffer opens",
})

-- ── Build, test, run ──────────────────────────────────────────────────────────
local function zig_ready()
  return exec.require_bin("zig", "See https://ziglang.org/download/")
end

---@param cmd string  shell command, run from the directory holding build.zig
local function run_in_build_root(cmd)
  local root = build_root()
  if not root then
    notify("build.zig not found above this file", vim.log.levels.WARN)
    return
  end
  term().float(term().cd_prefix(root) .. cmd)
end

local function run_file()
  local file = vim.fn.expand("%:p")
  if file == "" then
    notify("this buffer has no file on disk", vim.log.levels.WARN)
    return
  end
  if not zig_ready() then return end
  term().float("zig run " .. vim.fn.shellescape(file))
end

local function fetch_dependency()
  if not zig_ready() then return end
  if not build_root() then
    notify("build.zig not found above this file; zig fetch needs a Zig project", vim.log.levels.WARN)
    return
  end
  vim.ui.input({ prompt = "zig fetch URL: " }, function(url)
    url = vim.trim(url or "")
    if url == "" then return end
    run_in_build_root("zig fetch --save " .. vim.fn.shellescape(url))
  end)
end

--- Show `zig env` in a floating read-only buffer that is wiped when closed.
local function show_zig_env()
  if not zig_ready() then return end
  local res = vim.system({ "zig", "env" }, { text = true }):wait()
  if res.code ~= 0 or vim.trim(res.stdout or "") == "" then
    notify("`zig env` failed: " .. vim.trim(res.stderr or ""), vim.log.levels.ERROR)
    return
  end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].buftype   = "nofile"
  vim.bo[buf].swapfile  = false
  vim.api.nvim_buf_set_lines(buf, 0, -1, false,
    vim.split(res.stdout, "\n", { plain = true, trimempty = true }))
  vim.bo[buf].filetype   = "json"
  vim.bo[buf].modifiable = false

  local width  = math.min(80, vim.o.columns - 4)
  local height = math.max(3, math.min(vim.api.nvim_buf_line_count(buf), vim.o.lines - 6))
  vim.api.nvim_open_win(buf, true, {
    relative = "editor", style = "minimal", border = "rounded",
    width = width, height = height,
    row = math.floor((vim.o.lines - height) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    title = " zig env ", title_pos = "center",
  })
  vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf, silent = true, desc = "Close zig env" })
end

return {
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}
      opts.formatters.zigfmt = {
        command   = "zig",
        args      = { "fmt", "--stdin" },
        stdin     = true,
        condition = function() return vim.fn.executable("zig") == 1 end,
      }
    end,
  },

  shared.treesitter({ "zig" }),

  {
    "akinsho/toggleterm.nvim",
    keys = {
      { "<leader>zb", function() if zig_ready() then run_in_build_root("zig build run") end end,
        desc = "Zig Build Run", ft = "zig" },
      { "<leader>zt", function() if zig_ready() then run_in_build_root("zig build test") end end,
        desc = "Zig Build Test", ft = "zig" },
      { "<leader>zc", run_file,           desc = "Zig Run File",          ft = "zig" },
      { "<leader>zf", fetch_dependency,   desc = "Zig fetch (add dep)",   ft = "zig" },
      { "<leader>ze", show_zig_env,       desc = "Zig env info",          ft = "zig" },
    },
  },
}

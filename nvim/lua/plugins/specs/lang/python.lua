-- lua/plugins/specs/lang/python.lua — Python language support
--
-- Owns: venv-selector.nvim, nvim-dap-python (debugpy), iron.nvim (REPL), neogen docstrings,
-- PEP 8 indentation.
-- Escape hatches: vim.g.debugpy_python, vim.g.python_venv_auto_refresh, vim.g.python_docstring_style,
-- vim.g.python_probe_timeout_ms, vim.g.python_probe_ttl.
--

local bkm = require("core.util.buf_keymap")

local PROBE_SRC = "import debugpy, sys; sys.stdout.write(debugpy.__version__)"
local VENV_DIRNAMES = { ".venv", "venv", "env", ".env" }
local PROVISIONAL = "python3"
local DOCSTRING_STYLES = { google_docstrings = true, numpydoc = true, reST = true }
local DEFAULT_PROBE_TIMEOUT_MS = 4000
local DEFAULT_PROBE_TTL_S = 120

-- ── Settings ──────────────────────────────────────────────────────────────────

---@param name    string   vim.g flag
---@param default integer
---@param min     integer
---@return integer
local function int_flag(name, default, min)
  local v = vim.g[name]
  if type(v) == "number" and v >= min then return math.floor(v) end
  return default
end

---@param name    string
---@param default boolean
---@return boolean
local function bool_flag(name, default)
  local v = vim.g[name]
  if type(v) == "boolean" then return v end
  return default
end

---@return string
local function docstring_style()
  local v = vim.g.python_docstring_style
  if v == nil or DOCSTRING_STYLES[v] then return v or "google_docstrings" end
  vim.schedule(function()
    vim.notify(
      string.format(
        "[python] vim.g.python_docstring_style = %s is not one of: google_docstrings, numpydoc, reST. Using google_docstrings.",
        vim.inspect(v)
      ),
      vim.log.levels.WARN
    )
  end)
  return "google_docstrings"
end

-- ── Guards ────────────────────────────────────────────────────────────────────

--- Require `mod` and call `fn` with it. Errors surface as a notification instead of vanishing.
---@param mod string
---@param fn  fun(m: table)
---@return fun()
local function guarded(mod, fn)
  return function()
    local ok, m = pcall(require, mod)
    if not ok then
      vim.notify(string.format("[python] %s is not loaded — :Lazy install", mod), vim.log.levels.WARN)
      return
    end
    local ok2, err = pcall(fn, m)
    if not ok2 then vim.notify("[python] " .. tostring(err), vim.log.levels.WARN) end
  end
end

-- ── Interpreter discovery ─────────────────────────────────────────────────────

---@param dir string  a virtualenv / prefix directory
---@return string
local function venv_python(dir)
  if vim.fn.has("win32") == 1 then return dir .. "\\Scripts\\python.exe" end
  return dir .. "/bin/python"
end

---@return string
local function project_root()
  local ok, path = pcall(require, "core.util.path")
  return (ok and path.find_root()) or vim.fn.getcwd()
end

---@class PyCandidate
---@field path string  interpreter to probe
---@field why  string  label shown in messages

---@param root string
---@return PyCandidate[]  de-duplicated by real path, in priority order
local function candidates(root)
  local out, seen = {}, {}
  local function add(path, why)
    if type(path) ~= "string" or path == "" or vim.fn.executable(path) ~= 1 then return end
    local real = vim.uv.fs_realpath(path) or path
    if seen[real] then return end
    seen[real] = true
    out[#out + 1] = { path = path, why = why }
  end

  local pinned = vim.g.debugpy_python
  if type(pinned) == "string" and pinned ~= "" then add(vim.fn.expand(pinned), "vim.g.debugpy_python") end

  local active = vim.env.VIRTUAL_ENV
  if active and active ~= "" then add(venv_python(active), "$VIRTUAL_ENV") end

  for _, name in ipairs(VENV_DIRNAMES) do
    add(venv_python(root .. "/" .. name), "./" .. name)
  end

  local pyenv = vim.env.PYENV_ROOT
  if pyenv and pyenv ~= "" then
    local pinfile = vim.fn.findfile(".python-version", root .. ";")
    if pinfile ~= "" then
      local ver = vim.trim(vim.fn.readfile(pinfile)[1] or "")
      if ver ~= "" then add(pyenv .. "/versions/" .. ver .. "/bin/python", "pyenv " .. ver) end
    end
  end

  add(vim.fn.exepath("python3"), "PATH python3")
  add(vim.fn.exepath("python"), "PATH python")
  return out
end

--- Import debugpy in `path` and report its version. Always answers, and never synchronously.
---@param path       string
---@param timeout_ms integer
---@param done       fun(version: string|nil)
local function probe(path, timeout_ms, done)
  local settled = false
  local function settle(version)
    if settled then return end
    settled = true
    vim.schedule(function() done(version) end)
  end

  local ok, handle = pcall(vim.system, { path, "-c", PROBE_SRC }, { text = true, timeout = timeout_ms }, function(res)
    local v = res.code == 0 and vim.trim(res.stdout or "") or ""
    settle(v ~= "" and v or nil)
  end)
  if not ok then return settle(nil) end

  vim.defer_fn(function()
    if settled then return end
    pcall(handle.kill, handle, 9)
    settle(nil)
  end, timeout_ms + 500)
end

---@class PyResolution
---@field path    string
---@field why     string
---@field version string
---@field at      integer  os.time() at verification

---@type table<string, { hit: PyResolution|nil, tried: string[], at: integer }>
local _verdicts = {}

-- Concurrent requests for one root share a single probe run.
---@type table<string, fun(hit: PyResolution|nil, tried: string[])[]>
local _inflight = {}

---@param root string
---@param done fun(hit: PyResolution|nil, tried: string[])
local function resolve(root, done)
  local key = root .. "\0" .. (vim.env.VIRTUAL_ENV or "")
  local v = _verdicts[key]
  if v and (os.time() - v.at) < int_flag("python_probe_ttl", DEFAULT_PROBE_TTL_S, 1) then
    vim.schedule(function() done(v.hit, v.tried) end)
    return
  end
  if _inflight[key] then
    table.insert(_inflight[key], done)
    return
  end
  _inflight[key] = { done }

  local function finish(hit, tried)
    _verdicts[key] = { hit = hit, tried = tried, at = os.time() }
    local waiters = _inflight[key] or {}
    _inflight[key] = nil
    for _, w in ipairs(waiters) do w(hit, tried) end
  end

  local timeout = int_flag("python_probe_timeout_ms", DEFAULT_PROBE_TIMEOUT_MS, 100)
  local list, tried = candidates(root), {}
  local function step(i)
    if i > #list then return finish(nil, tried) end
    local c = list[i]
    probe(c.path, timeout, function(version)
      if not version then
        tried[#tried + 1] = string.format("%s (%s)", c.path, c.why)
        return step(i + 1)
      end
      finish({ path = c.path, why = c.why, version = version, at = os.time() }, tried)
    end)
  end
  step(1)
end

-- ── Adapter wiring ────────────────────────────────────────────────────────────

---@class PyWiring
---@field path    string
---@field why     string
---@field version string  debugpy version, or "unverified" for the provisional interpreter

---@type { entry: PyWiring|nil, configs_included: boolean }
local _wired = { entry = nil, configs_included = false }

--- Point nvim-dap-python's adapter at `path`. Transactional: on failure the previous adapters return.
---@param path    string
---@param why     string
---@param version string
---@return boolean ok
---@return string? err
local function wire(path, why, version)
  local ok_dap, dap = pcall(require, "dap")
  local ok_dp, dp = pcall(require, "dap-python")
  if not (ok_dap and ok_dp) then return false, "nvim-dap / nvim-dap-python not loadable" end

  local previous = { python = dap.adapters.python, debugpy = dap.adapters.debugpy }
  local ok, err = pcall(dp.setup, path, { include_configs = not _wired.configs_included })
  if not ok then
    dap.adapters.python, dap.adapters.debugpy = previous.python, previous.debugpy
    return false, tostring(err)
  end

  _wired.configs_included = true
  _wired.entry = { path = path, why = why, version = version }
  dp.resolve_python = function() return _wired.entry and _wired.entry.path or nil end
  return true
end

-- Roots already reported as "no debugpy" this session, so buffer switches do not repeat the warning.
local _reported = {}

-- The most recently requested root. A verdict for any other root is stale by the time it arrives
-- (the user has moved on), and must not re-wire the adapter.
local _latest_root = nil

---@param reason "startup"|"context"
local function sync(reason)
  local root = project_root()
  _latest_root = root
  resolve(root, function(hit, tried)
    if _latest_root ~= root then return end
    if not hit then
      if reason == "startup" or not _reported[root] then
        _reported[root] = true
        vim.notify(
          table.concat({
            "[python] no interpreter with debugpy for " .. vim.fn.fnamemodify(root, ":~"),
            "  checked: " .. (#tried > 0 and table.concat(tried, ", ") or "no interpreter on this machine"),
            "  fix:     <venv>/bin/python -m pip install debugpy, then :PythonDebugDoctor",
            "  or pin:  vim.g.debugpy_python = '/path/to/python'",
          }, "\n"),
          reason == "startup" and vim.log.levels.WARN or vim.log.levels.INFO
        )
      end
      return
    end
    _reported[root] = nil
    if _wired.entry and _wired.entry.path == hit.path then return end
    local ok, err = wire(hit.path, hit.why, hit.version)
    if not ok then
      vim.notify("[python] could not wire the debugpy adapter (" .. hit.path .. "): " .. err, vim.log.levels.WARN)
    elseif reason ~= "startup" then
      vim.notify(string.format("[python] debugpy %s via %s", hit.version, hit.why), vim.log.levels.INFO)
    end
  end)
end

local function on_context_change()
  if vim.bo.filetype ~= "python" or vim.bo.buftype ~= "" then return end
  sync("context")
end

local function start_debugpy()
  local ok, err = wire(PROVISIONAL, "provisional", "unverified")
  if not ok then vim.notify("[python] debugpy wiring failed: " .. err, vim.log.levels.WARN) end
  sync("startup")

  vim.api.nvim_create_autocmd({ "BufEnter", "DirChanged" }, {
    group = vim.api.nvim_create_augroup("PythonDebugpyResync", { clear = true }),
    callback = function() vim.schedule(on_context_change) end,
    desc = "Keep the debugpy adapter on the interpreter of the current project",
  })
end

-- ── Self-check ────────────────────────────────────────────────────────────────

local function doctor()
  local root = project_root()
  local ok_dap, dap = pcall(require, "dap")
  local ok_dp, dp = pcall(require, "dap-python")
  local rows = {}
  local function row(ok, label, detail)
    rows[#rows + 1] = string.format("  %s %-18s %s", ok and "✓" or "✗", label, detail or "")
  end

  row(ok_dp, "nvim-dap-python", ok_dp and "loaded" or "not loadable — :Lazy install nvim-dap-python")
  local adapter = ok_dap and type(dap.adapters.python) == "function"
  row(adapter, "adapter", adapter and "installed" or "missing — open a .py buffer first")
  local configs = ok_dap and #(dap.configurations.python or {}) or 0
  row(configs > 0, "launch configs", tostring(configs))
  row(ok_dp and dp.resolve_python ~= nil, "resolve_python", "debuggee shares the verified interpreter")
  local w = _wired.entry
  row(w ~= nil, "wired", w and string.format("%s (%s, debugpy %s)", w.path, w.why, w.version) or "none yet")
  row(
    true,
    "probe",
    string.format(
      "timeout %dms, cache TTL %ds",
      int_flag("python_probe_timeout_ms", DEFAULT_PROBE_TIMEOUT_MS, 100),
      int_flag("python_probe_ttl", DEFAULT_PROBE_TTL_S, 1)
    )
  )

  -- Verify afresh: drop this root's cached answer so the report reflects the machine now.
  for key in pairs(_verdicts) do
    if vim.startswith(key, root .. "\0") then _verdicts[key] = nil end
  end
  _reported[root] = nil
  resolve(root, function(hit, tried)
    row(
      hit ~= nil,
      "interpreter",
      hit and string.format("%s (%s, debugpy %s)", hit.path, hit.why, hit.version)
        or ("none with debugpy — checked: " .. (#tried > 0 and table.concat(tried, ", ") or "nothing"))
    )
    local out = { "Python debug doctor — " .. vim.fn.fnamemodify(root, ":~") }
    vim.list_extend(out, rows)
    vim.notify(table.concat(out, "\n"), hit and vim.log.levels.INFO or vim.log.levels.WARN)
  end)
end

vim.api.nvim_create_user_command("PythonDebugDoctor", doctor, {
  desc = "Verify the debugpy adapter wiring for this project",
})

-- ── Keymaps ───────────────────────────────────────────────────────────────────

local DAP_MAPS = {
  { "n", "<leader>pydm", guarded("dap-python", function(d) d.test_method() end), "Python debug method" },
  { "n", "<leader>pydc", guarded("dap-python", function(d) d.test_class() end), "Python debug class" },
  { { "n", "v" }, "<leader>pyds", guarded("dap-python", function(d) d.debug_selection() end), "Python debug selection" },
  { "n", "<leader>pydi", doctor, "Python debug doctor" },
}

--- Run `fn(iron.core)` with errors surfaced.
---@param fn fun(core: table)
---@return fun()
local function with_iron(fn)
  return guarded("iron.core", fn)
end

--- iron resolves the REPL from the current buffer, so these actions are only valid in Python buffers.
---@param fn fun()
---@return fun()
local function python_only(fn)
  return function()
    if vim.bo.filetype ~= "python" then
      vim.notify("[python] not a Python buffer — REPL action skipped", vim.log.levels.INFO)
      return
    end
    fn()
  end
end

-- ── REPL (iron.nvim) ──────────────────────────────────────────────────────────

---Interpreter command for the REPL: the verified environment's ipython, else any ipython, else python.
---@return string[]
local function repl_command()
  local py = _wired.entry and _wired.entry.path
  if py then
    local ipy = vim.fn.fnamemodify(py, ":h") .. "/ipython"
    if vim.fn.executable(ipy) == 1 then return { ipy, "--no-autoindent" } end
  end
  if vim.fn.executable("ipython") == 1 then return { "ipython", "--no-autoindent" } end
  return { py or "python3" }
end

local function setup_iron()
  local ok_core, core = pcall(require, "iron.core")
  local ok_common, common = pcall(require, "iron.fts.common")
  local ok_view, view = pcall(require, "iron.view")
  if not (ok_core and ok_common and ok_view) then
    vim.notify("[python] iron.nvim not loadable — :Lazy install Vigemus/iron.nvim", vim.log.levels.WARN)
    return
  end
  local ok, err = pcall(core.setup, {
    config = {
      scratch_repl = true,
      repl_definition = {
        python = { command = repl_command, format = common.bracketed_paste },
      },
      repl_open_cmd = view.bottom(20),
    },
    keymaps = {},
  })
  if not ok then vim.notify("[python] iron.nvim setup failed: " .. tostring(err), vim.log.levels.WARN) end
end

-- Operator-pending send: `<leader>pyrc{motion}`. The expr mapping sets operatorfunc and returns "g@",
-- so the operator reads our function only once the motion completes. The function restores the
-- previous operatorfunc as it runs, so another plugin's `g@` is not routed here. A cancelled operator
-- leaves ours set, which is harmless: every plugin sets its own operatorfunc before `g@`.
_G._nvim_iron_opfunc = function(motion)
  vim.o.operatorfunc = _G._nvim_iron_opfunc_prev or ""
  with_iron(function(core) core.send_motion(motion) end)()
end

local function iron_operator()
  _G._nvim_iron_opfunc_prev = vim.o.operatorfunc
  vim.o.operatorfunc = "v:lua._nvim_iron_opfunc"
  return "g@"
end

local function setup_iron_operator()
  local group = vim.api.nvim_create_augroup("PythonIronOperator", { clear = true })
  local function register(buf)
    vim.keymap.set("n", "<leader>pyrc", iron_operator, {
      buffer = buf, expr = true, silent = true, desc = "REPL send motion (operator)",
    })
  end
  vim.api.nvim_create_autocmd("FileType", {
    group = group, pattern = "python", callback = function(e) register(e.buf) end,
  })
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == "python" then register(buf) end
  end
end

local IRON_MAPS = {
  { "n", "<leader>pyrl", with_iron(function(c) c.send_line() end), "REPL send line" },
  { "v", "<leader>pyrv", with_iron(function(c) c.visual_send() end), "REPL send visual selection" },
  { "n", "<leader>pyru", with_iron(function(c) c.send_until_cursor() end), "REPL send until cursor" },
  { "n", "<leader>pyrq", with_iron(function(c) c.close_repl() end), "REPL quit" },
  { "n", "<leader>pyrx", python_only(with_iron(function(c) c.send(nil, string.char(12)) end)), "REPL clear" },
}

-- ── Specs ─────────────────────────────────────────────────────────────────────

return {
  -- ── Venv selector ──────────────────────────────────────────────────────────
  -- venv-selector reads its settings from `options`, `search`, `hooks` and `cache`. Top-level keys are
  -- silently ignored, which is how the previous `name` and `auto_refresh` options did nothing.
  {
    "linux-cultist/venv-selector.nvim",
    ft = "python",
    cmd = "VenvSelect",
    opts = {
      options = {
        cached_venv_automatic_activation = bool_flag("python_venv_auto_refresh", true),
      },
    },
    keys = {
      { "<leader>pyv", "<cmd>VenvSelect<cr>", desc = "Python select venv" },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("venv-selector").setup(opts) end)
      if not ok then
        vim.notify(
          "[python] venv-selector setup failed: " .. tostring(err) .. "\nRun :Lazy update venv-selector.nvim",
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── DAP: nvim-dap-python ───────────────────────────────────────────────────
  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    dependencies = "mfussenegger/nvim-dap",
    config = function()
      bkm.on_ft("python", DAP_MAPS, "python_dap_keymaps_registered")
      start_debugpy()
    end,
  },

  -- ── iron.nvim REPL ─────────────────────────────────────────────────────────
  {
    "Vigemus/iron.nvim",
    ft = "python",
    keys = {
      { "<leader>pyrs", "<cmd>IronRepl<cr>", desc = "Python REPL start" },
      { "<leader>pyrr", "<cmd>IronRestart<cr>", desc = "Python REPL restart" },
      {
        "<leader>pyri",
        python_only(with_iron(function(c) c.send(nil, string.char(3)) end)),
        desc = "Python REPL interrupt",
      },
    },
    config = function()
      setup_iron()
      setup_iron_operator()
      bkm.on_ft("python", IRON_MAPS, "python_iron_keymaps_registered")
    end,
  },

  -- ── Neogen docstrings ──────────────────────────────────────────────────────
  {
    "danymat/neogen",
    optional = true,
    ft = "python",
    opts = function(_, opts)
      opts.languages = opts.languages or {}
      opts.languages.python = { template = { annotation_convention = docstring_style() } }
    end,
    keys = {
      { "<leader>pyg", guarded("neogen", function(n) n.generate() end), desc = "Python generate docstring" },
    },
  },

  -- ── PEP 8 indent ───────────────────────────────────────────────────────────
  { "Vimjas/vim-python-pep8-indent", ft = "python" },
}

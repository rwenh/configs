-- lua/plugins/specs/lang/elixir.lua — Elixir: mix keymaps, toolchain checks, elixir-tools (NextLS, opt-in)
--

local shared = require("plugins.specs.lang.shared")
local term   = require("core.util.term")

local TTL = 60
local _toolchain = nil   -- { at, value }; global, because Hex lives under MIX_HOME
local _warned = {}       -- code -> true (one notice per session)

local function issue(list, code, msg, fix)
  list[#list + 1] = { code = code, msg = msg, fix = fix }
end

local function mix_home()
  local env = vim.env.MIX_HOME
  if type(env) == "string" and env ~= "" then return env end
  return (vim.env.HOME or vim.fn.expand("~")) .. "/.mix"
end

--- Toolchain readiness, cached for TTL seconds.
local function toolchain()
  local now = os.time()
  if _toolchain and (now - _toolchain.at) < TTL then return _toolchain.value end
  local v = { mix = vim.fn.executable("mix") == 1, hex = false, issues = {} }
  if not v.mix then
    issue(v.issues, "no-mix", "`mix` not found on PATH; Elixir LSP, tests and formatting will not work",
      "install Elixir: https://elixir-lang.org/install.html")
  else
    v.hex = #vim.fn.glob(mix_home() .. "/archives/hex-*", false, true) > 0
    if not v.hex then
      issue(v.issues, "no-hex", "Hex is not installed under " .. mix_home(), "mix local.hex --force")
    end
  end
  _toolchain = { at = now, value = v }
  return v
end

--- Only NextLS is checked here, because its binary is a per-user setting rather than part of the toolchain.
local function nextls_issue()
  if vim.g.elixir_use_nextls ~= true then return nil end
  local bin = vim.g.nextls_bin or "nextls"
  if vim.fn.executable(bin) == 1 then return nil end
  return {
    code = "no-nextls",
    msg  = "elixir_use_nextls is set but `" .. bin .. "` is not executable",
    fix  = "download nextls from https://github.com/elixir-tools/next-ls/releases, or unset elixir_use_nextls",
  }
end

local function report(issues)
  for _, i in ipairs(issues) do
    if not _warned[i.code] then
      _warned[i.code] = true
      vim.notify(("[elixir] %s\n  fix: %s"):format(i.msg, i.fix), vim.log.levels.WARN)
    end
  end
end

-- ── Mix projects and actions ───────────────────────────────────────────────

--- Nearest mix.exs above `buf`, or nil.
local function mix_root(buf)
  local ok_name, name = pcall(vim.api.nvim_buf_get_name, buf or 0)
  local dir = (ok_name and name ~= "") and vim.fn.fnamemodify(name, ":p:h") or vim.fn.getcwd()
  local ok, path = pcall(require, "core.util.path")
  if not ok then return nil end
  return path.find_up({ "mix.exs" }, dir)
end

local ACTIONS = {
  { lhs = "<leader>ext", args = { "test" },                                 desc = "Elixir mix test" },
  { lhs = "<leader>exC", args = { "test", "--cover" },                      desc = "Elixir mix test --cover" },
  { lhs = "<leader>exw", args = { "test", "--listen-on-stdin", "--stale" }, desc = "Elixir mix test --stale (watch)" },
  { lhs = "<leader>exf", args = { "format" },                               desc = "Elixir mix format" },
  { lhs = "<leader>exp", args = { "phx.server" },                           desc = "Elixir Phoenix server" },
  { lhs = "<leader>exi", raw = "iex -S mix",                                desc = "Elixir IEx session" },
  { lhs = "<leader>exd", args = { "deps.get" },                             desc = "Elixir mix deps.get" },
  { lhs = "<leader>exc", args = { "compile" },                              desc = "Elixir mix compile" },
}

local function run_action(buf, action)
  if not toolchain().mix then
    vim.notify("[elixir] mix not found on PATH — install Elixir first", vim.log.levels.WARN)
    return
  end
  local root = mix_root(buf)
  if not root then
    vim.notify("[elixir] no mix.exs above this file — not inside a Mix project", vim.log.levels.WARN)
    return
  end
  local cmd = action.raw
  if not cmd then
    local parts = { "mix" }
    for _, a in ipairs(action.args) do parts[#parts + 1] = vim.fn.shellescape(a) end
    cmd = table.concat(parts, " ")
  end
  term.float(term.cd_prefix(root) .. cmd)
end

local function build_keys()
  local out = {}
  for _, a in ipairs(ACTIONS) do
    out[#out + 1] = { a.lhs, function() run_action(0, a) end, desc = a.desc, ft = "elixir" }
  end
  return out
end

-- ── Autocmds (file scope) ──────────────────────────────────────────────────

vim.api.nvim_create_autocmd("FileType", {
  group    = vim.api.nvim_create_augroup("ElixirToolchain", { clear = true }),
  pattern  = { "elixir", "eex", "heex" },
  callback = function()
    local issues = vim.list_extend({}, toolchain().issues)
    local n = nextls_issue()
    if n then issues[#issues + 1] = n end
    report(issues)
  end,
  desc = "Report missing Elixir tooling once per session",
})

return {
  {
    "elixir-tools/elixir-tools.nvim",
    ft           = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },
    cond         = function() return vim.g.elixir_use_nextls == true end,
    config = function()
      local ok, elixir = pcall(require, "elixir")
      if not ok then
        vim.notify("[elixir] elixir-tools.nvim did not load: " .. tostring(elixir)
          .. "\nRun :Lazy update elixir-tools.nvim", vim.log.levels.WARN)
        return
      end
      local ok_setup, err = pcall(elixir.setup, {
        elixirls = { enable = false },
        nextls   = { enable = true, cmd = vim.g.nextls_bin or "nextls" },
      })
      if not ok_setup then
        vim.notify("[elixir] elixir-tools setup failed: " .. tostring(err), vim.log.levels.WARN)
      end
    end,
  },

  {
    "akinsho/toggleterm.nvim",
    ft   = "elixir",
    keys = build_keys(),
  },

  shared.treesitter({ "elixir", "heex", "eex" }),
}

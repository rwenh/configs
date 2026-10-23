-- lua/plugins/specs/lang/ruby.lua — Ruby and Rails: vim-test, vim-rails, generator keymaps, bundler checks
--

local shared = require("plugins.specs.lang.shared")
local bkm    = require("core.util.buf_keymap")
local term   = require("core.util.term")

local TTL  = 30
local FLAG = "ruby_rails_keymaps"

local _status = {}   -- root -> { at, key, value }
local _warned = {}   -- root .. "\0" .. code -> true (one notice per session)

-- ── Detection (filesystem only; no subprocess) ─────────────────────────────

local function dir_of(buf)
  local ok, name = pcall(vim.api.nvim_buf_get_name, buf or 0)
  if ok and name ~= "" then return vim.fn.fnamemodify(name, ":p:h") end
  return vim.fn.getcwd()
end

local function find_up(markers, dir)
  local ok, path = pcall(require, "core.util.path")
  if not ok then return nil end
  return path.find_up(markers, dir)
end

local function is_file(p)
  local st = vim.uv.fs_stat(p)
  return st ~= nil and st.type == "file"
end

local function stat_key(p)
  local st = vim.uv.fs_stat(p)
  return st and (st.mtime.sec .. ":" .. st.size) or "absent"
end

--- Rails app root for `buf`: nearest ancestor holding config/application.rb (vim-rails' own signal).
local function rails_root(buf)
  return find_up({ "config/application.rb" }, dir_of(buf))
end

--- Bundler project root for `buf`: nearest Gemfile or Gemfile.lock, else the buffer's directory.
local function bundle_root(buf)
  return find_up({ "Gemfile", "Gemfile.lock" }, dir_of(buf)) or dir_of(buf)
end

-- ── Rails launcher and generator keymaps ───────────────────────────────────

--- Launcher prefix for the app at `root`. Order: bin/rails, bundler-managed rails, rails on PATH.
---@return string|nil prefix
---@return string|nil reason
local function rails_prefix(root)
  if is_file(root .. "/bin/rails") and vim.fn.executable(root .. "/bin/rails") == 1 then
    return "./bin/rails"
  end
  if is_file(root .. "/Gemfile") and vim.fn.executable("bundle") == 1 then
    return "bundle exec rails"
  end
  if vim.fn.executable("rails") == 1 then return "rails" end
  return nil, "no Rails launcher: expected bin/rails, bundler with a Gemfile, or rails on PATH"
end

--- Run `rails <args>` from the Rails root of `buf` in a float terminal.
local function rails(buf, args)
  local root = rails_root(buf)
  if not root then
    vim.notify("[ruby] not inside a Rails app (no config/application.rb above this file)", vim.log.levels.WARN)
    return
  end
  local prefix, why = rails_prefix(root)
  if not prefix then
    vim.notify("[ruby] " .. why, vim.log.levels.WARN)
    return
  end
  local escaped = {}
  for _, a in ipairs(args) do escaped[#escaped + 1] = vim.fn.shellescape(a) end
  term.float(term.cd_prefix(root) .. prefix .. " " .. table.concat(escaped, " "))
end

local function ask_generate(buf, kind)
  vim.ui.input({ prompt = "rails generate " .. kind .. ": " }, function(input)
    local args = vim.split(vim.trim(input or ""), "%s+", { trimempty = true })
    if #args == 0 then return end
    rails(buf, vim.list_extend({ "generate", kind }, args))
  end)
end

local GENERATORS = {
  { "<leader>rgm", "model" }, { "<leader>rgc", "controller" }, { "<leader>rgs", "scaffold" },
  { "<leader>rgj", "job" },   { "<leader>rgM", "migration" },
}

local function rails_maps(buf)
  local maps = {}
  for _, g in ipairs(GENERATORS) do
    maps[#maps + 1] = { "n", g[1], function() ask_generate(buf, g[2]) end, "Rails generate " .. g[2] }
  end
  maps[#maps + 1] = { "n", "<leader>rgS", function() rails(buf, { "server" }) end,     "Rails server" }
  maps[#maps + 1] = { "n", "<leader>rgd", function() rails(buf, { "db:migrate" }) end, "Rails db:migrate" }
  maps[#maps + 1] = { "n", "<leader>rgr", function() rails(buf, { "console" }) end,    "Rails console" }
  return maps
end

--- Make one buffer's Rails keymaps match whether it sits in a Rails app. Verified after the write.
local function sync_rails_keymaps(buf)
  if not vim.api.nvim_buf_is_valid(buf) or vim.bo[buf].filetype ~= "ruby" then return end
  local want = rails_root(buf) ~= nil
  if (vim.b[buf][FLAG] == true) == want then return end
  if want then
    bkm.batch(buf, rails_maps(buf), FLAG)
  else
    bkm.clear(buf, FLAG, rails_maps(buf))
  end
  if (vim.b[buf][FLAG] == true) ~= want then
    vim.notify("[ruby] could not update the Rails keymaps for this buffer", vim.log.levels.WARN)
  end
end

-- ── Bundler / debugger readiness ───────────────────────────────────────────

local function mason_has(bin)
  local ok, mason = pcall(require, "core.util.mason")
  return ok and mason.bin_ok(bin) or false
end

--- Readiness for the project at `root`. Mirrors dap.lua's ruby adapter, which runs `bundle exec rdbg`
local function bundler_status(root)
  local lock = root .. "/Gemfile.lock"
  local key  = stat_key(lock)
  local now  = os.time()
  local hit  = _status[root]
  if hit and hit.key == key and (now - hit.at) < TTL then return hit.value end

  local v = {
    lockfile  = is_file(lock),
    bundle    = vim.fn.executable("bundle") == 1,
    debug_gem = false,
    rdbg      = vim.fn.executable("rdbg") == 1 or mason_has("rdbg"),
    issues    = {},
  }
  if v.lockfile then
    local ok, lines = pcall(vim.fn.readfile, lock)
    if ok then
      for _, line in ipairs(lines) do
        if line:match("^    debug %(") then v.debug_gem = true; break end
      end
    end
  end

  local function add(code, level, msg, fix)
    v.issues[#v.issues + 1] = { code = code, level = level, msg = msg, fix = fix }
  end
  -- Only projects that declare dependencies get checks; a loose script in a random directory does not.
  local has_gemfile = v.lockfile or is_file(root .. "/Gemfile")
  if has_gemfile then
    if v.lockfile and not v.bundle then
      add("no-bundler", vim.log.levels.WARN,
        "Gemfile.lock is present but bundler is not installed; rspec and rubocop will use the wrong gems",
        "gem install bundler")
    end
    if v.bundle and not (v.lockfile and v.debug_gem) then
      add("dap-bundle-mismatch", vim.log.levels.WARN,
        "bundler is installed, so Ruby DAP runs `bundle exec rdbg`, but this project has no debug gem in Gemfile.lock",
        "bundle add debug --group development (or set up a Gemfile for this project)")
    end
    if not v.bundle and not v.rdbg then
      add("no-rdbg", vim.log.levels.INFO, "rdbg not found; Ruby debugging is unavailable", "gem install debug")
    end
  end

  _status[root] = { at = now, key = key, value = v }
  return v
end

local function report(root, issues)
  for _, i in ipairs(issues) do
    local k = root .. "\0" .. i.code
    if not _warned[k] then
      _warned[k] = true
      vim.notify(("[ruby] %s\n  fix: %s"):format(i.msg, i.fix), i.level)
    end
  end
end

local function check_project(buf)
  local root = bundle_root(buf)
  report(root, bundler_status(root).issues)
end

-- ── vim-test ───────────────────────────────────────────────────────────────

local function run_in_terminal(cmd)
  term.float(cmd)
end

local function rkey(lhs, cmd, desc)
  return { lhs, "<cmd>" .. cmd .. "<cr>", desc = desc, ft = "ruby" }
end

-- ── Autocmds (file scope: registered once when the spec loads) ─────────────

local group = vim.api.nvim_create_augroup("RubyProjectState", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group    = group,
  pattern  = "ruby",
  callback = function(e)
    sync_rails_keymaps(e.buf)
    check_project(e.buf)
  end,
  desc = "Rails keymaps and bundler checks for Ruby buffers",
})

vim.api.nvim_create_autocmd("DirChanged", {
  group    = group,
  callback = function()
    vim.schedule(function()
      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(b) and vim.bo[b].filetype == "ruby" then sync_rails_keymaps(b) end
      end
    end)
  end,
  desc = "Reconcile Rails keymaps for every open Ruby buffer",
})

for _, b in ipairs(vim.api.nvim_list_bufs()) do   -- buffers that already exist when this spec loads
  if vim.api.nvim_buf_is_loaded(b) and vim.bo[b].filetype == "ruby" then sync_rails_keymaps(b) end
end

return {
  {
    "vim-test/vim-test",
    ft   = "ruby",
    init = function()
      vim.g["test#strategy"] = "toggleterm"
      local custom = vim.g["test#custom_strategies"]
      vim.g["test#custom_strategies"] = vim.tbl_extend(
        "force", type(custom) == "table" and custom or {}, { toggleterm = run_in_terminal })
    end,
    keys = {
      rkey("<leader>rbn", "TestNearest", "Ruby Test Nearest"),
      rkey("<leader>rbf", "TestFile",    "Ruby Test File"),
      rkey("<leader>rbs", "TestSuite",   "Ruby Test Suite"),
      rkey("<leader>rbl", "TestLast",    "Ruby Test Last"),
      rkey("<leader>rbv", "TestVisit",   "Ruby Test Visit"),
    },
  },

  -- No startup cond: vim-rails detects Rails per buffer (b:rails_root) and is a no-op elsewhere.
  { "tpope/vim-rails", ft = { "ruby", "eruby" } },

  {
    "RRethy/nvim-treesitter-endwise",
    event        = "InsertEnter",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },

  shared.treesitter({ "ruby" }),
}

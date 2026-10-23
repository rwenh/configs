-- lua/core/util/dburl.lua — database connection resolution for vim-dadbod and vim-dadbod-ui
--
-- Owns g:db. Sources, highest precedence first:
--   1. g:db set by anything other than this module          (manual; sticky across projects)
--   2. $DATABASE_URL / $DB_URL / $DATABASE_URI               (process environment; global)
--   3. DATABASE_URL / DB_URL / DATABASE_URI in <root>/.env   (.env, .env.local, .env.development)
--

local M = {}

local STATE   = "_nvim_db_source"    -- { url, kind, name, label, root } of the URL we wrote last
local CLEARED = "_nvim_db_cleared"   -- project root where the user cleared the connection
local TTL     = 30                   -- seconds a dotenv read is trusted without re-checking
local MAX_DOTENV_BYTES = 64 * 1024
local MAX_URL_BYTES    = 2048

local DEFAULT_ENV_VARS = { "DATABASE_URL", "DB_URL", "DATABASE_URI" }
local DEFAULT_DOTENV   = { ".env", ".env.local", ".env.development" }

local DIALECT_BY_SCHEME = {
  postgres = "pg", postgresql = "pg", pgsql = "pg", pg = "pg",
  mysql = "mysql", mariadb = "mysql",
  sqlite = "sqlite", sqlite3 = "sqlite",
  mssql = "mssql", sqlserver = "mssql",
}

-- Each query lists user tables and views only, so it is safe to run as-is.
local SCHEMA_QUERY = {
  pg = [[SELECT table_schema, table_name, table_type
FROM information_schema.tables
WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
ORDER BY table_schema, table_name;]],
  mysql = "SHOW TABLES;",
  sqlite = [[SELECT name, type FROM sqlite_master
WHERE type IN ('table', 'view') AND name NOT LIKE 'sqlite_%'
ORDER BY name;]],
  mssql = [[SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE
FROM INFORMATION_SCHEMA.TABLES
ORDER BY TABLE_SCHEMA, TABLE_NAME;]],
}

local function list_flag(name, default)
  local v = vim.g[name]
  if type(v) == "table" and #v > 0 then return v end
  return default
end

-- ── Pure helpers (no editor state) ─────────────────────────────────────────

--- Parse one dotenv line: `KEY=value`, `export KEY=value`, quoted values, trailing comments.
---@param line string
---@return string|nil key
---@return string|nil value  nil for comments, junk, or a quote that never closes
function M.parse_dotenv_line(line)
  line = (line or ""):gsub("\r$", "")
  local key, rest = line:match("^%s*export%s+([%w_]+)%s*=(.*)$")
  if not key then key, rest = line:match("^%s*([%w_]+)%s*=(.*)$") end
  if not key then return nil, nil end
  rest = vim.trim(rest)
  local q = rest:sub(1, 1)
  if q == '"' or q == "'" then
    local close = rest:find(q, 2, true)
    if not close then return nil, nil end
    return key, rest:sub(2, close - 1)
  end
  local value = rest:gsub("%s+#.*$", "")
  return key, vim.trim(value)
end

--- Structural check. A dadbod URL starts with a scheme and contains no whitespace or control bytes.
---@param url any
---@return boolean ok
---@return string? reason
function M.validate_url(url)
  if type(url) ~= "string" or url == "" then return false, "empty value" end
  if #url > MAX_URL_BYTES then return false, ("longer than %d bytes"):format(MAX_URL_BYTES) end
  if url:find("[%c%s]") then return false, "contains whitespace or control characters" end
  if not url:match("^%a[%w+.-]*:") then
    return false, "no scheme (expected something like postgres://user@host/db)"
  end
  return true, nil
end

---@param url any
---@return string|nil dialect  "pg" | "mysql" | "sqlite" | "mssql", or nil when unknown
function M.dialect(url)
  if type(url) ~= "string" then return nil end
  local scheme = url:match("^(%a[%w+.-]*):")
  return scheme and DIALECT_BY_SCHEME[scheme:lower()] or nil
end

---@param url any
---@return string|nil sql  a system-table-free listing of tables and views, or nil when unknown
function M.schema_query(url)
  local d = M.dialect(url)
  return d and SCHEMA_QUERY[d] or nil
end

-- ── Project root ───────────────────────────────────────────────────────────

---@param buf integer?
---@return string
function M.project_root(buf)
  local ok_name, name = pcall(vim.api.nvim_buf_get_name, buf or 0)
  local dir = (ok_name and name ~= "") and vim.fn.fnamemodify(name, ":p:h") or vim.fn.getcwd()
  local ok, path = pcall(require, "core.util.path")
  local found = ok and path.find_up({ ".git", ".env", ".env.local", ".env.development" }, dir) or nil
  return found or dir
end

-- ── Dotenv reads (mtime + size + TTL memo; a miss is cached as a table too) ─

local _dotenv = {}

local function dotenv_lookup(file, names)
  local st = vim.uv.fs_stat(file)
  if not st or st.type ~= "file" or st.size > MAX_DOTENV_BYTES then return nil, nil end
  local memo_key = file .. "\0" .. table.concat(names, ",")
  local now = os.time()
  local hit = _dotenv[memo_key]
  if hit and hit.mtime == st.mtime.sec and hit.size == st.size and (now - hit.at) < TTL then
    return hit.key, hit.url
  end
  local wanted = {}
  for _, n in ipairs(names) do wanted[n] = true end
  local key, url
  local ok, lines = pcall(vim.fn.readfile, file)
  if ok then
    for _, line in ipairs(lines) do
      local k, v = M.parse_dotenv_line(line)
      if k and wanted[k] and v and v ~= "" then key, url = k, v; break end
    end
  end
  _dotenv[memo_key] = { at = now, mtime = st.mtime.sec, size = st.size, key = key, url = url }
  return key, url
end

--- First URL available for `root`, or nil. Writes nothing.
---@return table|nil cand  { url, kind, name, label, root? }
local function candidate(root)
  local names = list_flag("db_url_env_vars", DEFAULT_ENV_VARS)
  for _, name in ipairs(names) do
    local raw = vim.env[name]
    local v = type(raw) == "string" and vim.trim(raw) or ""
    if v ~= "" then return { url = v, kind = "env", name = name, label = "$" .. name } end
  end
  for _, f in ipairs(list_flag("db_url_dotenv_files", DEFAULT_DOTENV)) do
    local file = root .. "/" .. f
    local key, url = dotenv_lookup(file, names)
    if key then
      return {
        url = url, kind = "dotenv", name = key, root = root,
        label = vim.fn.fnamemodify(file, ":~:.") .. " (" .. key .. ")",
      }
    end
  end
  return nil
end

-- ── g:db state ─────────────────────────────────────────────────────────────

--- The source of the URL in g:db, if this module wrote it and nothing changed it since.
---@return table|nil
function M.current()
  local s = vim.g[STATE]
  if type(s) ~= "table" or type(s.url) ~= "string" then return nil end
  if vim.g.db ~= s.url then return nil end
  return s
end

local function unset()
  vim.g.db = nil
  vim.g[STATE] = nil
  return vim.g.db == nil
end

local function apply(cand)
  local ok, why = M.validate_url(cand.url)
  if not ok then
    vim.notify(("[database] ignored the URL from %s: %s"):format(cand.label, why), vim.log.levels.WARN)
    return nil
  end
  vim.g.db = cand.url
  if vim.g.db ~= cand.url then   -- verify-after-write
    unset()
    vim.notify("[database] could not set g:db from " .. cand.label, vim.log.levels.ERROR)
    return nil
  end
  vim.g[STATE] = { url = cand.url, kind = cand.kind, name = cand.name, root = cand.root, label = cand.label }
  if not M.current() then        -- verify the bookkeeping as well
    unset()
    return nil
  end
  vim.notify("[database] connection from " .. cand.label, vim.log.levels.INFO)
  return M.current()
end

--- Make g:db agree with the environment for `root`. Idempotent; safe on every DirChanged.
---@param root string
---@return table|nil source  the source in effect afterwards, or nil
function M.sync(root)
  -- A clear lasts only while you stay in the project that was cleared.
  if vim.g[CLEARED] and vim.g[CLEARED] ~= root then vim.g[CLEARED] = nil end

  local ours = M.current()
  -- A dotenv URL belongs to the project it came from.
  if ours and ours.kind == "dotenv" and ours.root ~= root then
    unset()
    ours = nil
  end
  -- Anything in g:db that this module did not write is a manual choice. Leave it alone.
  if not ours and type(vim.g.db) == "string" and vim.g.db ~= "" then return nil end
  if vim.g[CLEARED] == root then return nil end

  local cand = candidate(root)
  if not cand then
    if ours then unset() end
    return nil
  end
  if ours and ours.url == cand.url then return ours end
  return apply(cand)
end

--- Drop the connection until you leave this project. Returns ok, optional note for the user.
---@param root string
---@return boolean ok
---@return string? note
function M.clear(root)
  unset()
  vim.g[CLEARED] = root
  if vim.g.db ~= nil then return false, "g:db is still set" end
  local raw = vim.env.DATABASE_URL
  if type(raw) == "string" and vim.trim(raw) ~= "" then
    return true, "$DATABASE_URL still applies to :DB, which falls back to it"
  end
  return true, nil
end

--- Diagnostics summary: where the active connection came from and what kind of database it is.
---@return table
function M.status()
  local s = M.current()
  if not s then
    local set = type(vim.g.db) == "string" and vim.g.db ~= ""
    return { set = set, source = set and "manual" or nil }
  end
  return { set = true, source = s.kind, label = s.label, dialect = M.dialect(s.url) }
end

return M

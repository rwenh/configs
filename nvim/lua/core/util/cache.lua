-- lua/core/util/cache.lua — the one TTL + file-signature memo used by the config
--
-- The hand-rolled caches this replaces shared three defects: a nil return that meant
-- both "not computed" and "computed as nothing" (so misses re-ran on every call), no
-- check that the file had changed, and no invalidation on :cd. A cache here stores an
-- explicit entry table, so a legitimate nil or false is remembered, and a file-backed
-- entry is reused only while its signature (mtime, size, type) is unchanged.
--
-- Invalidation: caches clear on DirChanged unless created with on_dirchange = false.
-- Entries also expire after their TTL; an entry may carry its own TTL (math.huge = never).

local M = {}

-- Neovim embeds LuaJIT, which has neither table.pack nor table.unpack.
local unpack = unpack or table.unpack
local function pack(...)
  local t = { ... }
  t.n = select("#", ...)
  return t
end

local DEFAULT_TTL = 30
local _instances = {}

--- Signature of a path: "absent", or mtime (sec and nsec), size and type.
---@param path string
---@return string
function M.file_sig(path)
  local st = vim.uv.fs_stat(path)
  if not st then
    return "absent"
  end
  return ("%d.%d:%d:%s"):format(st.mtime.sec, st.mtime.nsec, st.size, st.type)
end

---@class Cache
---@field name string
---@field ttl integer|number   default lifetime of an entry, in seconds
---@field on_dirchange boolean
---@field _entries table<string, { at: integer, ttl: number, sig: string|nil, value: any }>
local Cache = {}
Cache.__index = Cache

--- Read an entry. Returns hit (boolean) and value. A stored nil is a hit with value nil.
---@param key string
---@param sig string|nil  when given, the entry is a hit only if its signature matches
---@return boolean hit
---@return any value
function Cache:lookup(key, sig)
  local e = self._entries[key]
  if not e then
    return false, nil
  end
  if (os.time() - e.at) >= e.ttl or (sig ~= nil and e.sig ~= sig) then
    self._entries[key] = nil
    return false, nil
  end
  return true, e.value
end

---@param key   string
---@param value any
---@param sig   string|nil  file signature the value was computed from
---@param ttl   number|nil  overrides the cache default for this entry
function Cache:set(key, value, sig, ttl)
  self._entries[key] = {
    at = os.time(),
    ttl = ttl or self.ttl,
    sig = sig,
    value = value,
  }
end

--- Return the cached result for `key` while the file at `path` is unchanged, or compute and store it.
--- `compute` may return several values; all of them are cached and returned together.
--- opts.ttl     overrides the entry lifetime
--- opts.ttl_for function(...) returning the lifetime for this result (e.g. long for success, short for failure)
---@param key     string
---@param path    string
---@param compute fun(): ...
---@param opts    { ttl: number?, ttl_for: (fun(...): number)? }?
---@return ...
function Cache:memo(key, path, compute, opts)
  local sig = M.file_sig(path)
  local e = self._entries[key]
  if e and e.sig == sig and (os.time() - e.at) < e.ttl then
    return unpack(e.value, 1, e.value.n)
  end
  local packed = pack(compute())
  local ttl = (opts and opts.ttl) or (opts and opts.ttl_for and opts.ttl_for(unpack(packed, 1, packed.n))) or nil
  self:set(key, packed, sig, ttl)
  return unpack(packed, 1, packed.n)
end

function Cache:clear(key)
  self._entries[key] = nil
end

function Cache:clear_all()
  self._entries = {}
end

---@param opts { name: string?, ttl: number?, on_dirchange: boolean? }?
---@return Cache
function M.new(opts)
  opts = opts or {}
  local ttl = (type(opts.ttl) == "number" and opts.ttl >= 0) and opts.ttl or DEFAULT_TTL
  local self = setmetatable({
    name = opts.name or "anonymous",
    ttl = ttl,
    on_dirchange = opts.on_dirchange ~= false,
    _entries = {},
  }, Cache)
  table.insert(_instances, self)
  return self
end

--- Clear every cache that is scoped to the working directory.
function M.clear_all()
  for _, c in ipairs(_instances) do
    if c.on_dirchange then
      c:clear_all()
    end
  end
end

vim.api.nvim_create_autocmd("DirChanged", {
  group = vim.api.nvim_create_augroup("NvimIdeCacheDirChanged", { clear = true }),
  callback = M.clear_all,
  desc = "Clear working-directory caches (project roots, project configs) on :cd",
})

return M

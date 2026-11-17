-- lua/core/util/lspreg.lua — the one path that registers a language server with Neovim
--
-- Every server this config starts goes through M.register:
--   * binary check: a server whose binary is missing is skipped and listed once. Set `optional = true`
--     to skip the notice, for servers that are nice to have;
--   * blink.cmp capabilities;
--   * on_attach: the caller's function, then vim.g.lsp_on_attach_overrides[server];
--   * project overrides from .lspconfig.lua, read for the server's own root in before_init. They apply
--     `settings` and `init_options` only. `cmd`, `filetypes` and other fields are global and need a restart.
--
-- Servers another plugin starts (jdtls, rustaceanvim, typescript-tools, elixir-tools) own their clients
-- and are configured in their own specs.

local flags = require("core.flags")
local packages = require("core.util.packages")
local projconf = require("core.util.projconf")

local M = {}

local _caps -- memoised only once blink.cmp has answered
local _missing = {} -- "server (binary)" entries
local _reported = 0 -- how many entries of _missing have been shown
local _warned_roots = {} -- root -> true, one notice per bad .lspconfig.lua

local function capabilities()
  if _caps then
    return _caps
  end
  local ok, blink = pcall(require, "blink.cmp")
  if ok then
    local ok_caps, caps = pcall(blink.get_lsp_capabilities)
    if ok_caps and type(caps) == "table" and caps.textDocument ~= nil then
      _caps = caps
      return _caps
    end
  end
  return vim.lsp.protocol.make_client_capabilities()
end

-- nvim-lspconfig resolves html/css/json/yaml/tailwind from a project's node_modules/.bin, so the cwd's
-- copy counts as well. Checked when the server is registered.
local function available(bin)
  return vim.fn.executable(bin) == 1 or vim.fn.executable(vim.fn.getcwd() .. "/node_modules/.bin/" .. bin) == 1
end

--- Overrides for `server` from the .lspconfig.lua of `root`, or nil. Trust-gated by projconf.
---@param root   string|nil
---@param server string
---@return table|nil
local function overrides_for(root, server)
  if not root or root == "" then
    return nil
  end
  local res = projconf.load(root, ".lspconfig.lua")
  if res.ok then
    local ov = res.value[server]
    return type(ov) == "table" and ov or nil
  end
  if not res.absent and not _warned_roots[root] then
    _warned_roots[root] = true
    vim.notify("[lsp] .lspconfig.lua not used in " .. root .. ": " .. res.reason, vim.log.levels.WARN)
  end
  return nil
end

---@param server string  lspconfig name
---@param config table?  vim.lsp.Config fields, plus `optional` (boolean)
---@return boolean registered
function M.register(server, config)
  config = vim.deepcopy(config or {})
  local optional = config.optional == true
  config.optional = nil

  local bin = (config.cmd and config.cmd[1]) or packages.bin(server)
  if bin and not available(bin) then
    if not optional then
      _missing[#_missing + 1] = server .. " (" .. bin .. ")"
      vim.schedule(M.report_missing)
    end
    return false
  end

  config.capabilities = vim.tbl_deep_extend("force", capabilities(), config.capabilities or {})

  local user_init = config.before_init
  config.before_init = function(params, cfg)
    if type(user_init) == "function" then
      pcall(user_init, params, cfg)
    end
    local ov = overrides_for(cfg and cfg.root_dir, server)
    if not ov then
      return
    end
    if ov.settings then
      cfg.settings = vim.tbl_deep_extend("force", cfg.settings or {}, ov.settings)
    end
    if ov.init_options then
      local merged = vim.tbl_deep_extend("force", params.initializationOptions or {}, ov.init_options)
      params.initializationOptions = merged
      cfg.init_options = merged
    end
  end

  local user_attach = config.on_attach
  config.on_attach = function(client, bufnr)
    if type(user_attach) == "function" then
      pcall(user_attach, client, bufnr)
    end
    local overrides = flags.get("lsp_on_attach_overrides", nil)
    local fn = type(overrides) == "table" and overrides[server] or nil
    if type(fn) == "function" then
      pcall(fn, client, bufnr)
    end
  end

  vim.lsp.config(server, config)
  local ok, err = pcall(vim.lsp.enable, server)
  if not ok then
    vim.notify(("[lsp] %s failed to enable: %s"):format(server, tostring(err)), vim.log.levels.ERROR)
    return false
  end
  return true
end

--- Show the servers skipped since the last call, once each.
function M.report_missing()
  if #_missing <= _reported then
    return
  end
  local fresh = vim.list_slice(_missing, _reported + 1)
  _reported = #_missing
  vim.notify(
    ("[lsp] %d language server(s) not on PATH, skipped: %s\nInstall them with :MasonInstallAll (or :Mason)."):format(
      #fresh,
      table.concat(fresh, ", ")
    ),
    vim.log.levels.INFO
  )
end

return M

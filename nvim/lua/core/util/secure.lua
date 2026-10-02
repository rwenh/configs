-- lua/core/util/secure.lua — trusted loading of per-project Lua config files
--
-- Project files (.lspconfig.lua, .runner.lua, .nvim-dap.lua, .overseer.lua) are
-- arbitrary Lua executed with your privileges. Loading them with dofile() means
-- merely opening a cloned repo can run its code. This goes through Neovim's trust
-- database instead: you are prompted once per file+content-hash, and any change
-- to the file re-prompts. Manage with :trust.
--
-- Escape hatch: vim.g.disable_project_configs = true  (never load project files)

local M = {}

---@return boolean
function M.enabled()
  return vim.g.disable_project_configs ~= true
end

--- Load a trusted Lua file and return its result.
---@param path string
---@return boolean ok
---@return any     result_or_error
function M.load(path)
  if not M.enabled() then
    return false, "project configs disabled (vim.g.disable_project_configs)"
  end
  if vim.fn.filereadable(path) ~= 1 then return false, "not readable: " .. path end

  local content = vim.secure and vim.secure.read(path) or nil
  if content == nil then
    return false, "not trusted — run `:trust` on that file to allow it"
  end

  local loader = loadstring or load
  local chunk, err = loader(content, "@" .. path)
  if not chunk then return false, "parse error: " .. tostring(err) end
  return pcall(chunk)
end

return M

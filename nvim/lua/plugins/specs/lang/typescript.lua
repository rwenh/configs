-- lua/plugins/specs/lang/typescript.lua — TypeScript development (typescript-tools.nvim)
--

local shared = require("plugins.specs.lang.shared")
local jsonc  = require("core.util.jsonc")

local notify = function(msg, level) vim.notify("[ts] " .. msg, level or vim.log.levels.INFO) end

local TSCONFIG_CANDIDATES = { "tsconfig.json", "tsconfig.base.json", "tsconfig.app.json", "tsconfig.node.json" }

-- Files that never belong in a barrel: tests, stories, declaration files.
local BARREL_EXCLUDE = { "%.test%.[tj]sx?$", "%.spec%.[tj]sx?$", "%.stories%.[tj]sx?$", "%.d%.ts$" }

-- ── tsconfig ──────────────────────────────────────────────────────────────────

---@param file string
---@param depth integer?
---@return table|nil
local function load_tsconfig(file, depth)
  depth = depth or 0
  local cfg = jsonc.read(file)
  if not cfg then return nil end
  local has_paths = cfg.compilerOptions and cfg.compilerOptions.paths
  if type(cfg.extends) == "string" and cfg.extends:sub(1, 1) == "." and depth < 5 and not has_paths then
    local base = vim.fs.normalize(vim.fn.fnamemodify(file, ":h") .. "/" .. cfg.extends)
    if not base:match("%.json$") then base = base .. ".json" end
    local parent = load_tsconfig(base, depth + 1)
    if parent then
      cfg.compilerOptions = vim.tbl_deep_extend("keep", cfg.compilerOptions or {}, parent.compilerOptions or {})
    end
  end
  return cfg
end

---@param root string
---@return string|nil
local function find_tsconfig(root)
  for _, name in ipairs(TSCONFIG_CANDIDATES) do
    local f = root .. "/" .. name
    if vim.fn.filereadable(f) == 1 then return f end
  end
  return nil
end

-- ── Barrel generation ─────────────────────────────────────────────────────────

---@param dir string
---@return string[]
local function barrel_lines(dir)
  local files = vim.fn.globpath(dir, "*.ts", false, true)
  vim.list_extend(files, vim.fn.globpath(dir, "*.tsx", false, true))

  local stems, seen = {}, {}
  for _, f in ipairs(files) do
    local base = vim.fn.fnamemodify(f, ":t")
    local stem = vim.fn.fnamemodify(f, ":t:r")
    local excluded = base == "index.ts" or base == "index.tsx"
    for _, pat in ipairs(BARREL_EXCLUDE) do
      if f:match(pat) then excluded = true end
    end
    -- foo.ts and foo.tsx would produce the same export twice.
    if not excluded and not seen[stem] then
      seen[stem] = true
      table.insert(stems, stem)
    end
  end
  table.sort(stems)

  local lines = {}
  for i, stem in ipairs(stems) do
    lines[i] = 'export * from "./' .. stem .. '";'
  end
  return lines
end

local function write_barrel()
  local dir = vim.fn.expand("%:p:h")
  local exports = barrel_lines(dir)
  if #exports == 0 then
    notify("No exportable files found in " .. dir, vim.log.levels.WARN)
    return
  end

  local barrel = dir .. "/index.ts"
  local function do_write()
    -- vim.fn.writefile raises on failure (E482), so it must run under pcall.
    local ok = pcall(vim.fn.writefile, exports, barrel)
    if not ok or vim.fn.filereadable(barrel) ~= 1 then
      notify("Failed to write " .. barrel, vim.log.levels.ERROR)
      return
    end
    notify("Barrel written: " .. barrel)
    vim.cmd("edit " .. vim.fn.fnameescape(barrel))
  end

  if vim.fn.filereadable(barrel) == 1 then
    vim.ui.input({ prompt = "index.ts already exists — overwrite? [y/N]: " }, function(ans)
      if ans == "y" or ans == "Y" then
        do_write()
      else
        notify("Barrel generation cancelled.")
      end
    end)
  else
    do_write()
  end
end

-- ── Paths aliases ─────────────────────────────────────────────────────────────

local function show_paths()
  local root = shared.project_root()
  local tsconfig = find_tsconfig(root)
  if not tsconfig then
    notify("tsconfig.json not found", vim.log.levels.WARN)
    return
  end
  local cfg = load_tsconfig(tsconfig)
  if not cfg then
    notify("Could not parse " .. vim.fn.fnamemodify(tsconfig, ":t") .. " — check for syntax errors.", vim.log.levels.WARN)
    return
  end
  local paths = cfg.compilerOptions and cfg.compilerOptions.paths
  if not paths or vim.tbl_isempty(paths) then
    notify("No paths aliases defined in " .. vim.fn.fnamemodify(tsconfig, ":t"))
    return
  end

  local names = vim.tbl_keys(paths)
  table.sort(names)
  local out = { "tsconfig paths aliases (" .. vim.fn.fnamemodify(tsconfig, ":t") .. "):" }
  for _, alias in ipairs(names) do
    local targets = paths[alias]
    local first = type(targets) == "table" and targets[1] or tostring(targets)
    table.insert(out, string.format("  %-30s → %s", alias, first))
  end
  notify(table.concat(out, "\n"))
end

local function jump_tsconfig()
  local f = find_tsconfig(shared.project_root())
  if f then
    vim.cmd("edit " .. vim.fn.fnameescape(f))
  else
    notify("No tsconfig.json found in project root", vim.log.levels.WARN)
  end
end

return {
  {
    "pmizio/typescript-tools.nvim",
    ft           = shared.JS_TS_FT,
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      settings = {
        tsserver_file_preferences = {
          importModuleSpecifierPreference         = vim.g.ts_import_preference or "non-relative",
          includeInlayParameterNameHints          = "literals",
          includeInlayVariableTypeHints           = false,
          includeInlayFunctionLikeReturnTypeHints = true,
        },
      },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("typescript-tools").setup(opts) end)
      if not ok then notify("typescript-tools setup failed: " .. tostring(err), vim.log.levels.WARN) end
    end,
    keys = {
      { "<leader>tso", "<cmd>TSToolsOrganizeImports<cr>",       desc = "TS Organize Imports",      ft = shared.JS_TS_FT },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",     desc = "TS Add Missing Imports",   ft = shared.JS_TS_FT },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>",   desc = "TS Remove Unused Imports", ft = shared.JS_TS_FT },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",                desc = "TS Fix All Auto-fixable",  ft = shared.JS_TS_FT },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>",  desc = "TS Go To Source Def",      ft = shared.JS_TS_FT },
      { "<leader>tsc", jump_tsconfig,  desc = "TS Jump to tsconfig.json",                      ft = shared.JS_TS_FT },
      { "<leader>tsb", write_barrel,   desc = "TS Generate barrel (index.ts)",                 ft = shared.JS_TS_FT },
      { "<leader>tsp", show_paths,     desc = "TS Show tsconfig paths aliases",                ft = shared.JS_TS_FT },
    },
  },

  shared.treesitter({ "typescript", "tsx" }),
}

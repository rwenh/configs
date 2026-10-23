-- lua/plugins/specs/lang/javascript.lua — JavaScript development
--

local shared = require("plugins.specs.lang.shared")

-- ── Module system (ESM / CJS) ─────────────────────────────────────────────────
local MODULE_TTL = 30
local _module_cache = {}

local function read_package_type(root)
  local pkg = root .. "/package.json"
  if vim.fn.filereadable(pkg) ~= 1 then return nil end
  local ok_read, lines = pcall(vim.fn.readfile, pkg)
  if not ok_read then return nil end
  local ok_json, obj = pcall(vim.json.decode, table.concat(lines, "\n"))
  if not ok_json or type(obj) ~= "table" then return nil end
  return obj.type == "module" and "esm" or "cjs"
end

---@param root string?
---@return "esm"|"cjs"|nil  nil when there is no package.json
local function module_kind(root)
  root = root or shared.project_root()
  local now = os.time()
  local cached = _module_cache[root]
  if cached and (now - cached.at) < MODULE_TTL then return cached.kind or nil end
  local kind = read_package_type(root)
  _module_cache[root] = { at = now, kind = kind or false }
  return kind
end

local function is_esm() return module_kind() == "esm" end
local function is_cjs() return module_kind() ~= "esm" end

-- ── Node version (async, once per session) ────────────────────────────────────
local _node_checked = false

local function check_node_version()
  if _node_checked then return end
  _node_checked = true
  if vim.fn.executable("node") ~= 1 then return end
  require("core.util.exec").version_async("node", { "node", "--version" }, function(ver)
    local major = ver and tonumber(ver:match("^(%d+)"))
    if major and major < 18 then
      vim.notify(
        string.format(
          "[js] Node %s detected — version 18+ recommended for LSP servers.\nUpgrade: https://nodejs.org",
          ver
        ),
        vim.log.levels.WARN
      )
    end
  end)
end

vim.api.nvim_create_autocmd("FileType", {
  pattern  = shared.JS_FT,
  once     = true,
  group    = vim.api.nvim_create_augroup("JsEnvCheck", { clear = true }),
  callback = function() vim.schedule(check_node_version) end,
})

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ────────
require("core.util.snippets").register("javascript", function(s, t, i, _, ref)
  return {
    -- Two snippets share the trigger; LuaSnip expands whichever condition holds at the cursor.
    s({ trig = "imp", name = "import (ESM)", dscr = "ESM default import",
        condition = is_esm, show_condition = is_esm }, {
      t("import "), i(1, "name"), t(" from '"), i(2, "module"), t("'"),
    }),
    s({ trig = "imp", name = "require (CJS)", dscr = "CommonJS require",
        condition = is_cjs, show_condition = is_cjs }, {
      t("const "), i(1, "name"), t(" = require('"), i(2, "module"), t("')"),
    }),
    s("afn", {
      t("const "), i(1, "name"), t(" = async ("), i(2), t(") => {"),
      t({ "", "  " }), i(0), t({ "", "}" }),
    }),
    s("pall", {
      t("const ["), i(1, "a"), t(", "), i(2, "b"), t("] = await Promise.all(["),
      t({ "", "  " }), ref(1, "a"), t(","),
      t({ "", "  " }), ref(2, "b"), t(","),
      t({ "", "])" }),
    }),
    s("tc", {
      t("try {"), t({ "", "  " }), i(1, "// ..."),
      t({ "", "} catch (" }), i(2, "err"), t(") {"),
      t({ "", "  console.error(" }), ref(2, "err"), t(")"),
      t({ "", "}" }),
    }),
    s("cl", { t('console.log("'), i(1, "label"), t(':", '), i(2, "value"), t(")") }),
    s("rcl", { t("// console.log("), i(0), t(")") }),
  }
end)

-- ── Module-type notification ──────────────────────────────────────────────────
local function notify_module_type()
  local kind = module_kind()
  local label = ({
    esm = 'ES Modules (type: "module")',
    cjs = "CommonJS (default)",
  })[kind] or "unknown (no package.json found)"
  vim.notify("[js] Module system: " .. label, vim.log.levels.INFO)
end

-- ── package-info.nvim ─────────────────────────────────────────────────────────
local function package_info(fn_name)
  return function()
    local ok, pi = pcall(require, "package-info")
    if not ok then
      vim.notify("[js] package-info.nvim is not loaded — run :Lazy", vim.log.levels.WARN)
      return
    end
    local ok_call, err = pcall(pi[fn_name])
    if not ok_call then
      vim.notify("[js] package-info " .. fn_name .. " failed: " .. tostring(err), vim.log.levels.ERROR)
    end
  end
end

return {
  {
    "vuki656/package-info.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    event = "BufRead package.json",
    cond  = function()
      return not vim.fn.expand("%:p"):find("node_modules", 1, true)
    end,
    opts = {
      highlights = {
        up_to_date = { fg = "#3C4048" },
        outdated   = { fg = "#d19a66" },
      },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("package-info").setup(opts) end)
      if not ok then
        vim.notify("[js] package-info setup failed: " .. tostring(err), vim.log.levels.WARN)
      end
    end,
    keys = {
      { "<leader>jps", package_info("show"),            desc = "Show package versions" },
      { "<leader>jpu", package_info("update"),          desc = "Update package"        },
      { "<leader>jpd", package_info("delete"),          desc = "Delete package"        },
      { "<leader>jpi", package_info("install"),         desc = "Install package"       },
      { "<leader>jpc", package_info("change_version"),  desc = "Change version"        },
    },
  },

  {
    "akinsho/toggleterm.nvim",
    ft = shared.JS_FT,
    keys = {
      { "<leader>jsm", notify_module_type, desc = "JS Show module type (ESM / CJS)", ft = shared.JS_FT },
    },
  },

  shared.treesitter({ "javascript", "jsdoc" }),
}

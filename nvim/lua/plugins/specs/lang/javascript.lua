-- lua/plugins/specs/lang/javascript.lua — JavaScript development
--

local shared = require("plugins.specs.lang.shared")

local _module_type_cache = {}

local function detect_module_type()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return nil end

  if _module_type_cache[root] ~= nil then
    return _module_type_cache[root] or nil   -- return nil for false sentinel
  end

  local pkg_file = root .. "/package.json"
  if vim.fn.filereadable(pkg_file) ~= 1 then
    _module_type_cache[root] = false   -- sentinel: no package.json
    return nil
  end

  local ok_read, lines = pcall(vim.fn.readfile, pkg_file)
  if not ok_read then
    _module_type_cache[root] = false
    return nil
  end

  local content = table.concat(lines, "\n")
  local ok_json, pkg = pcall(vim.json.decode, content)
  if not ok_json or type(pkg) ~= "table" then
    _module_type_cache[root] = false
    return nil
  end

  local t = (pkg["type"] == "module") and "esm" or "cjs"
  _module_type_cache[root] = t
  return t
end

local function notify_module_type()
  local t = detect_module_type()
  if not t then
    vim.notify("[js] No package.json found — module type unknown", vim.log.levels.INFO)
  else
    local label = t == "esm" and "ES Modules (type: \"module\")" or "CommonJS (default)"
    vim.notify("[js] Module system: " .. label, vim.log.levels.INFO)
  end
end

vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("JsModuleTypeCache", { clear = true }),
  callback = function() _module_type_cache = {} end,
})

local _node_checked = false
local function check_node_version()
  if _node_checked then return end
  _node_checked = true
  if vim.fn.executable("node") ~= 1 then return end
  local out = vim.trim(vim.fn.system("node --version 2>/dev/null"))
  if out == "" then return end
  local major = tonumber(out:match("v(%d+)"))
  if major and major < 18 then
    vim.notify(
      "[js] Node " .. out .. " detected — version 18+ recommended for LSP servers.\n"
      .. "Upgrade: https://nodejs.org",
      vim.log.levels.WARN
    )
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
    opts  = { colors = { up_to_date = "#3C4048", outdated = "#d19a66" } },
    config = function(_, opts)
      local ok, err = pcall(function() require("package-info").setup(opts) end)
      if not ok then
        vim.notify("[javascript] package-info setup failed: " .. tostring(err), vim.log.levels.WARN)
      end
    end,
    keys = {
      { "<leader>jps", function() pcall(function() require("package-info").show()           end) end, desc = "Show package versions" },
      { "<leader>jpu", function() pcall(function() require("package-info").update()         end) end, desc = "Update package"        },
      { "<leader>jpd", function() pcall(function() require("package-info").delete()         end) end, desc = "Delete package"        },
      { "<leader>jpi", function() pcall(function() require("package-info").install()        end) end, desc = "Install package"       },
      { "<leader>jpc", function() pcall(function() require("package-info").change_version() end) end, desc = "Change version"        },
    },
  },

  {
    "akinsho/toggleterm.nvim",
    ft = shared.JS_FT,
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = shared.JS_FT,
        once     = true,
        group    = vim.api.nvim_create_augroup("JsEnvCheck", { clear = true }),
        callback = function() vim.schedule(check_node_version) end,
      })
    end,
    keys = {
      { "<leader>jsm", function() notify_module_type() end, desc = "JS Show module type (ESM / CJS)", ft = shared.JS_FT },
    },
  },

  {
    "L3MON4D3/LuaSnip", optional = true, ft = { "javascript", "javascriptreact" },
    config = function()
      require("core.util.snippets").load("javascript", function(s, t, i, _, ref)
        local module_type = detect_module_type()
        local use_esm     = (module_type == "esm")
        return {
          s("imp", use_esm and {
            t("import "), i(1, "name"), t(" from '"), i(2, "module"), t("'"),
          } or {
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
    end,
  },

  shared.treesitter({ "javascript", "jsdoc" }),
}

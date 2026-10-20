-- lua/plugins/specs/lang/kotlin.lua — Kotlin development
--
-- Owns: Kotlin build, run, test and Spring Boot keymaps, KDoc through Neogen, and :KotlinDoctor.
-- The language server and debug adapter are wired in lsp.lua and dap.lua.
-- Escape hatch: vim.g.kotlin_spring_cache_ttl (seconds; 0 disables the Spring detection cache).
--
-- Design:
--   * Build, test, run and Spring Boot commands go through core.util.jvm, which maps each logical
--     task to the right Gradle or Maven goal.
--   * A file with no build file is compiled with kotlinc into stdpath("cache")/kotlin-run. The old
--     code wrote <file>.jar next to the source, which left artifacts in the working tree.
--   * Spring Boot detection is cached per project root for the TTL. The old DirChanged expiry only
--     touched stale entries and did nothing for fresh ones.
--   * One project resolver serves every keymap, instead of a copy inside each one.

local shared = require("plugins.specs.lang.shared")
local jvm = require("core.util.jvm")

local WARN, INFO = vim.log.levels.WARN, vim.log.levels.INFO

---@param name    string
---@param default integer
---@param min     integer
---@return integer
local function int_flag(name, default, min)
  local v = vim.g[name]
  if type(v) == "number" and v >= min then return math.floor(v) end
  return default
end

-- ── Project ───────────────────────────────────────────────────────────────────

---@return string
local function project_root()
  local ok, path = pcall(require, "core.util.path")
  return (ok and path.find_root()) or vim.fn.getcwd()
end

---@type table<string, { at: integer, value: boolean }>
local _spring = {}

---@param root string
---@return boolean
local function is_spring(root)
  local ttl = int_flag("kotlin_spring_cache_ttl", 300, 0)
  local entry = _spring[root]
  if ttl > 0 and entry and (os.time() - entry.at) < ttl then return entry.value end
  local value = jvm.is_spring_project(root)
  _spring[root] = { at = os.time(), value = value }
  return value
end

-- ── Single-file builds (no Gradle or Maven project) ───────────────────────────

---@param file string
---@return string jar  under stdpath("cache"), never beside the source
local function single_file_jar(file)
  local dir = vim.fn.stdpath("cache") .. "/kotlin-run"
  vim.fn.mkdir(dir, "p")
  return string.format("%s/%s-%s.jar", dir, vim.fn.sha256(file):sub(1, 12), vim.fn.fnamemodify(file, ":t:r"))
end

---@param file string
---@param run  boolean
---@return string
local function single_file_cmd(file, run)
  local jar = single_file_jar(file)
  local cmd = string.format("kotlinc %s -include-runtime -d %s", vim.fn.shellescape(file), vim.fn.shellescape(jar))
  if run then cmd = cmd .. " && java -jar " .. vim.fn.shellescape(jar) end
  return cmd
end

---@param file string
---@return string
local function compile_only(file) return single_file_cmd(file, false) end

---@param file string
---@return string
local function compile_and_run(file) return single_file_cmd(file, true) end

-- ── Dispatch ──────────────────────────────────────────────────────────────────

--- Run a logical task for the current project. Without a Gradle or Maven build, `fallback` (if given)
--- builds the current file instead.
---@param task     JvmTask
---@param opts     table|nil
---@param fallback (fun(file: string): string)|nil
local function dispatch(task, opts, fallback)
  local cmd, err = jvm.command(project_root(), task)
  if not cmd and fallback then
    local file = vim.api.nvim_buf_get_name(0)
    if file ~= "" then cmd = fallback(file) end
  end
  if not cmd then
    vim.notify("[kotlin] " .. tostring(err) .. ". kotlinc is needed for single-file builds.", WARN)
    return
  end
  require("core.util.term").float(cmd, opts)
end

---@param task JvmTask
---@param opts table|nil
local function spring(task, opts)
  local root = project_root()
  if not is_spring(root) then
    vim.notify("[kotlin] no Spring Boot dependency declared in " .. vim.fn.fnamemodify(root, ":~"), WARN)
    return
  end
  dispatch(task, opts, nil)
end

---@param kind "func"|"class"
---@return fun()
local function kdoc(kind)
  return function()
    local ok, neogen = pcall(require, "neogen")
    if not ok then
      vim.notify("[kotlin] neogen is not loadable — :Lazy install neogen", WARN)
      return
    end
    local ok2, err = pcall(neogen.generate, { type = kind })
    if not ok2 then vim.notify("[kotlin] KDoc: " .. tostring(err), WARN) end
  end
end

-- ── Doctor ────────────────────────────────────────────────────────────────────

local function doctor()
  local root = project_root()
  local mason = require("core.util.mason")
  local tool = jvm.build_tool(root) or "none"
  local spring_declared = is_spring(root)
  local rows = {}
  local function row(ok, label, detail)
    rows[#rows + 1] = string.format("  %s %-22s %s", ok and "✓" or "✗", label, detail or "")
  end

  row(tool ~= "none", "build tool", tool == "none" and "no Gradle or Maven build file" or tool)
  row(spring_declared, "spring boot", spring_declared and "declared in the build file" or "not declared")
  row(vim.fn.executable("kotlinc") == 1, "kotlinc", "needed for single-file builds")
  row(mason.bin_ok("kotlin-language-server"), "kotlin-language-server", "LSP — :MasonInstall kotlin-language-server")
  row(mason.bin_ok("kotlin-debug-adapter"), "kotlin-debug-adapter", "DAP — :MasonInstall kotlin-debug-adapter")
  row(mason.bin_ok("ktlint"), "ktlint", "formatter — :MasonInstall ktlint")

  local out = { "Kotlin doctor — " .. vim.fn.fnamemodify(root, ":~") }
  vim.list_extend(out, rows)
  vim.notify(table.concat(out, "\n"), INFO)
end

vim.api.nvim_create_user_command("KotlinDoctor", doctor, {
  desc = "Report Kotlin build, Spring Boot and tooling status",
})

-- ── Keymaps ───────────────────────────────────────────────────────────────────

local function kt(lhs, fn, desc)
  return { lhs, fn, desc = desc, ft = "kotlin" }
end

local function keys()
  return {
    kt("<leader>ktb", function() dispatch("build", nil, compile_only) end, "Kotlin build"),
    kt("<leader>ktt", function() dispatch("test", nil, nil) end, "Kotlin test"),
    kt("<leader>ktr", function() dispatch("run", nil, compile_and_run) end, "Kotlin run"),
    kt("<leader>kts", function() spring("spring-run", { close_on_exit = false }) end, "Kotlin Spring Boot run"),
    kt("<leader>ktS", function() spring("spring-package", nil) end, "Kotlin Spring Boot package"),
    kt("<leader>ktd", kdoc("func"), "Kotlin generate KDoc (function)"),
    kt("<leader>ktD", kdoc("class"), "Kotlin generate KDoc (class)"),
  }
end

-- ── Specs ─────────────────────────────────────────────────────────────────────

return {
  shared.treesitter({ "kotlin" }),

  {
    "akinsho/toggleterm.nvim",
    keys = keys(),
  },
}

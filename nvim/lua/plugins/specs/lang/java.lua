-- lua/plugins/specs/lang/java.lua — Java development (nvim-jdtls)
--
-- Owns: the jdtls client, java-debug and java-test bundles, Java refactor, test and Spring Boot
-- keymaps, and Neogen Javadoc. Escape hatch: vim.g.jdtls_heap_mb (initial heap of the jdtls JVM).
--

local bkm = require("core.util.buf_keymap")
local jvm = require("core.util.jvm")

local MIN_JAVA = 17
local JAVA_TTL_S = 600
local BUNDLE_TTL_S = 60
local ROOT_MARKERS = {
  ".git", "mvnw", "gradlew", "pom.xml",
  "build.gradle", "build.gradle.kts", "settings.gradle", "settings.gradle.kts",
}
local ERROR, WARN, INFO = vim.log.levels.ERROR, vim.log.levels.WARN, vim.log.levels.INFO

---@param name    string
---@param default integer
---@param min     integer
---@return integer
local function int_flag(name, default, min)
  local v = vim.g[name]
  if type(v) == "number" and v >= min then return math.floor(v) end
  return default
end

-- ── Facts ─────────────────────────────────────────────────────────────────────

---@type { at: integer, major: integer|nil }
local _java = { at = 0, major = nil }

---@return integer|nil
local function java_major()
  if _java.at > 0 and (os.time() - _java.at) < JAVA_TTL_S then return _java.major end
  local major
  if vim.fn.executable("java") == 1 then
    local ok, res = pcall(function()
      return vim.system({ "java", "-version" }, { text = true, timeout = 5000 }):wait()
    end)
    if ok and res then
      -- `java -version` writes to stderr. "1.8.0" is Java 8; "17.0.2" is Java 17.
      local text = (res.stderr or "") .. (res.stdout or "")
      major = tonumber(text:match('version "1%.(%d+)') or text:match('version "(%d+)'))
    end
  end
  _java = { at = os.time(), major = major }
  return major
end

---@return { root: string, launcher: string|nil, config_dir: string }
local function jdtls_layout()
  local root = require("core.util.mason").pkg("jdtls")
  local launcher = vim.fn.glob(root .. "/plugins/org.eclipse.equinox.launcher_*.jar", false, true)[1]
  local os_name = (vim.uv.os_uname() or {}).sysname or "Linux"
  local flavour = os_name:find("Windows") and "config_win" or (os_name == "Darwin" and "config_mac" or "config_linux")
  return { root = root, launcher = launcher, config_dir = root .. "/" .. flavour }
end

---@type { at: integer, debug: string[], test: string[] }|nil
local _bundles = nil

---@return { debug: string[], test: string[] }
local function bundles()
  if _bundles and (os.time() - _bundles.at) < BUNDLE_TTL_S then return _bundles end
  local mason = require("core.util.mason")
  _bundles = {
    at = os.time(),
    debug = vim.fn.glob(mason.pkg("java-debug-adapter") .. "/extension/server/com.microsoft.java.debug.plugin-*.jar", false, true),
    test = jvm.filter_test_bundles(vim.fn.glob(mason.pkg("java-test") .. "/extension/server/*.jar", false, true)),
  }
  return _bundles
end

---@return table
local function facts()
  local b = bundles()
  return {
    java = vim.fn.executable("java") == 1,
    javac = vim.fn.executable("javac") == 1,
    major = java_major(),
    jdtls = jdtls_layout(),
    debug = b.debug,
    test = b.test,
  }
end

---@param f table
---@return { level: integer, msg: string }[]
local function problems(f)
  local out = {}
  local function add(level, msg) out[#out + 1] = { level = level, msg = msg } end

  if not f.java then
    add(ERROR, "java is not on PATH. Install a JDK " .. MIN_JAVA .. "+: sudo zypper in java-" .. MIN_JAVA .. "-openjdk-devel")
  elseif not f.major then
    add(WARN, "could not read a version from `java -version`; jdtls may fail to start")
  elseif f.major < MIN_JAVA then
    add(ERROR, string.format("java %d is too old for jdtls (needs %d+). Install a newer JDK.", f.major, MIN_JAVA))
  end
  if f.java and not f.javac then
    add(WARN, "javac not found: jdtls needs a JDK, not a JRE. Install java-" .. MIN_JAVA .. "-openjdk-devel")
  end
  if not f.jdtls.launcher then
    add(ERROR, "jdtls is not installed. Run: :MasonInstall jdtls")
  elseif vim.fn.isdirectory(f.jdtls.config_dir) ~= 1 then
    add(ERROR, "jdtls has no config directory for this OS: " .. f.jdtls.config_dir .. "\nRun: :MasonInstall jdtls")
  end
  if #f.debug == 0 then
    add(WARN, "java-debug-adapter is not installed, so Java debugging is unavailable. Run: :MasonInstall java-debug-adapter")
  end
  if #f.test == 0 then
    add(INFO, "java-test is not installed, so <leader>jvt and <leader>jvn are unavailable. Run: :MasonInstall java-test")
  end
  return out
end

-- Messages shown on the previous check. Only new ones are notified, so every attach stays quiet.
---@type table<string, true>
local _shown = {}

--- Notify on the transition into each problem. Returns true when a blocking problem is present.
---@param list { level: integer, msg: string }[]
---@return boolean blocking
local function report(list)
  local now, blocking = {}, false
  for _, p in ipairs(list) do
    now[p.msg] = true
    if p.level == ERROR then blocking = true end
    if not _shown[p.msg] then vim.notify("[java] " .. p.msg, p.level) end
  end
  _shown = now
  return blocking
end

---@param root string
---@return string
local function workspace_for(root)
  return vim.fn.stdpath("data") .. "/jdtls-workspace/" .. vim.fn.sha256(root)
end

-- ── Keymap helpers ────────────────────────────────────────────────────────────

---@param fn   fun(opts: any)
---@param opts any
local function call(fn, opts)
  local ok, err = pcall(fn, opts)
  if not ok then vim.notify("[java] " .. tostring(err), WARN) end
end

--- Refactors read the '< and '> marks, which only update when visual mode ends. Leave it first.
---@param fn fun(opts: any)
---@return fun()
local function visual_call(fn)
  return function()
    vim.api.nvim_feedkeys(vim.keycode("<Esc>"), "nx", false)
    call(fn, { visual = true })
  end
end

---@param bufnr integer
---@param jdtls table
local function ensure_dap(bufnr, jdtls)
  if vim.b[bufnr].jdtls_dap_ready then return end
  local ok, err = pcall(function()
    jdtls.setup_dap({ hotcodereplace = "auto" })   -- idempotent: returns early once the adapter exists
    require("jdtls.dap").setup_dap_main_class_configs()
  end)
  if ok then
    vim.b[bufnr].jdtls_dap_ready = true
  else
    vim.notify("[java] debug setup failed: " .. tostring(err), WARN)
  end
end

---@param root string
---@param task JvmTask
---@param opts table|nil
local function build_in_terminal(root, task, opts)
  local cmd, err = jvm.command(root, task)
  if not cmd then
    vim.notify("[java] " .. err, WARN)
    return
  end
  require("core.util.term").float(cmd, opts)
end

---@param bufnr    integer
---@param jdtls    table
---@param root     string
---@param has_test boolean
---@param is_spring boolean
---@return table[]
local function keymap_specs(bufnr, jdtls, root, has_test, is_spring)
  local maps = {
    { "n", "<leader>jvo", function() call(jdtls.organize_imports) end, "Java organize imports" },
    { "n", "<leader>jvv", function() call(jdtls.extract_variable, { visual = false }) end, "Java extract variable" },
    { "v", "<leader>jvv", visual_call(jdtls.extract_variable), "Java extract variable (selection)" },
    { "n", "<leader>jvc", function() call(jdtls.extract_constant, { visual = false }) end, "Java extract constant" },
    { "v", "<leader>jvc", visual_call(jdtls.extract_constant), "Java extract constant (selection)" },
    { "v", "<leader>jvm", visual_call(jdtls.extract_method), "Java extract method (selection)" },
    {
      "n", "<leader>jvR",
      function()
        local ok, err = pcall(jdtls.update_project_config)
        if ok then
          vim.notify("[java] project config reloaded", INFO)
        else
          vim.notify("[java] reload failed: " .. tostring(err), WARN)
        end
      end,
      "Java reload project config",
    },
  }

  if has_test then
    vim.list_extend(maps, {
      { "n", "<leader>jvt", function() ensure_dap(bufnr, jdtls); call(jdtls.test_class, { bufnr = bufnr }) end, "Java test class" },
      { "n", "<leader>jvn", function() ensure_dap(bufnr, jdtls); call(jdtls.test_nearest_method, { bufnr = bufnr }) end, "Java test nearest method" },
    })
  end

  if is_spring then
    vim.list_extend(maps, {
      { "n", "<leader>jvs", function() build_in_terminal(root, "spring-run", { close_on_exit = false }) end, "Java Spring Boot run" },
      { "n", "<leader>jvS", function() build_in_terminal(root, "spring-package", nil) end, "Java Spring Boot package" },
    })
  end

  return maps
end

---@param root     string
---@param jdtls    table
---@param has_test boolean
---@param is_spring boolean
---@return fun(client: table, bufnr: integer)
local function make_on_attach(root, jdtls, has_test, is_spring)
  return function(_, bufnr)
    -- Buffer-local and re-registered on every attach, so re-attaching replaces rather than duplicates.
    bkm.batch(bufnr, keymap_specs(bufnr, jdtls, root, has_test, is_spring))
    vim.api.nvim_create_autocmd("LspDetach", {
      group = vim.api.nvim_create_augroup("JdtlsDetach_" .. bufnr, { clear = true }),
      buffer = bufnr,
      once = true,
      callback = function(ev)
        local client = ev.data and vim.lsp.get_client_by_id(ev.data.client_id)
        if client and client.name == "jdtls" then vim.b[bufnr].jdtls_started = nil end
      end,
      desc = "Let the next Java open of this buffer attach jdtls again",
    })
  end
end

---@param f table
---@param root string
---@param jdtls table
---@param has_test boolean
---@param is_spring boolean
---@return table
local function build_config(f, root, jdtls, has_test, is_spring)
  local heap = int_flag("jdtls_heap_mb", 1024, 256)
  local bundle_list = vim.list_extend(vim.deepcopy(f.debug), f.test)
  return {
    cmd = {
      "java",
      "-Declipse.application=org.eclipse.jdt.ls.core.id1",
      "-Dosgi.bundles.defaultStartLevel=4",
      "-Declipse.product=org.eclipse.jdt.ls.core.product",
      "-Dlog.protocol=true",
      "-Dlog.level=WARNING",
      "-Xms" .. heap .. "m",
      "--add-modules=ALL-SYSTEM",
      "--add-opens", "java.base/java.util=ALL-UNNAMED",
      "--add-opens", "java.base/java.lang=ALL-UNNAMED",
      "-jar", f.jdtls.launcher,
      "-configuration", f.jdtls.config_dir,
      "-data", workspace_for(root),
    },
    root_dir = root,
    settings = {
      java = {
        signatureHelp = { enabled = true },
        completion = { enabled = true },
        format = { enabled = true },
      },
    },
    init_options = { bundles = bundle_list },
    on_attach = make_on_attach(root, jdtls, has_test, is_spring),
  }
end

--- Attach jdtls to one buffer. Idempotent until that buffer's jdtls client detaches.
---@param buf integer
local function start_for_buffer(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if vim.bo[buf].buftype ~= "" or vim.bo[buf].filetype ~= "java" then return end
  if vim.b[buf].jdtls_started then return end

  local ok_jdtls, jdtls = pcall(require, "jdtls")
  if not ok_jdtls then
    vim.notify("[java] nvim-jdtls is not loadable — :Lazy install nvim-jdtls", ERROR)
    return
  end

  local f = facts()
  if report(problems(f)) then return end

  local dir = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":p:h")
  local root = require("core.util.path").find_up(ROOT_MARKERS, dir) or dir
  local config = build_config(f, root, jdtls, #f.test > 0, jvm.is_spring_project(root))

  -- start_or_attach uses the current buffer unless told otherwise. Pass this one explicitly.
  local ok, err = pcall(jdtls.start_or_attach, config, nil, { bufnr = buf })
  if ok then
    vim.b[buf].jdtls_started = true
  else
    vim.notify(
      string.format("[java] jdtls failed to start: %s\nIf the workspace is corrupt, remove %s and reopen.",
        tostring(err), workspace_for(root)),
      WARN
    )
  end
end

-- ── Doctor ────────────────────────────────────────────────────────────────────

local function doctor()
  local f = facts()
  local rows = {}
  local function row(ok, label, detail)
    rows[#rows + 1] = string.format("  %s %-14s %s", ok and "✓" or "✗", label, detail or "")
  end

  row(f.java, "java", f.java and (f.major and ("major " .. f.major) or "version unknown") or "missing")
  row(f.javac, "javac", f.javac and "found" or "missing — a JDK is needed, not just a JRE")
  row(f.jdtls.launcher ~= nil, "jdtls", f.jdtls.launcher or ("missing under " .. f.jdtls.root .. " — :MasonInstall jdtls"))
  row(vim.fn.isdirectory(f.jdtls.config_dir) == 1, "jdtls config", f.jdtls.config_dir)
  row(#f.debug > 0, "java-debug", #f.debug > 0 and f.debug[1] or "missing — :MasonInstall java-debug-adapter")
  row(#f.test > 0, "java-test", #f.test > 0 and (#f.test .. " bundle(s)") or "missing — :MasonInstall java-test")

  local blocking = false
  for _, p in ipairs(problems(f)) do
    if p.level == ERROR then blocking = true end
  end
  local out = { "Java doctor" }
  vim.list_extend(out, rows)
  vim.notify(table.concat(out, "\n"), blocking and ERROR or INFO)
end

vim.api.nvim_create_user_command("JavaDoctor", doctor, {
  desc = "Report jdtls, JDK and debug/test bundle status",
})

-- ── Specs ─────────────────────────────────────────────────────────────────────

return {
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    dependencies = { "mfussenegger/nvim-dap", "williamboman/mason.nvim" },
    config = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "java",
        group = vim.api.nvim_create_augroup("JdtlsAttach", { clear = true }),
        callback = function(e) start_for_buffer(e.buf) end,
      })
      -- The buffer that triggered lazy-loading may not replay FileType, so cover every loaded Java buffer.
      -- The per-buffer guard makes a double call harmless.
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) then start_for_buffer(buf) end
      end
    end,
  },

  -- ── Neogen: Javadoc ────────────────────────────────────────────────────────
  {
    "danymat/neogen",
    optional = true,
    ft = "java",
    opts = { languages = { java = { template = { annotation_convention = "javadoc" } } } },
    keys = {
      {
        "<leader>jvg",
        function() call(function() require("neogen").generate() end) end,
        desc = "Java generate Javadoc",
        ft = "java",
      },
    },
  },
}

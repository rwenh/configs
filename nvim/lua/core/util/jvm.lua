-- lua/core/util/jvm.lua — JVM helpers shared by the Java, Kotlin and runner specs
--

local M = {}

local GRADLE_BUILD_FILES = { "build.gradle", "build.gradle.kts", "settings.gradle", "settings.gradle.kts" }
local MAVEN_BUILD_FILE   = "pom.xml"

local function is_file(p)
  return vim.fn.filereadable(p) == 1
end

local function is_exec_file(p)
  return is_file(p) and vim.fn.executable(p) == 1
end

-- ── Spring detection ──────────────────────────────────────────────────────────

---@param root string?
---@return boolean
function M.is_spring_project(root)
  root = root or vim.fn.getcwd()
  for _, fname in ipairs({ "build.gradle", "build.gradle.kts", "pom.xml" }) do
    local f = root .. "/" .. fname
    if is_file(f) then
      for _, line in ipairs(vim.fn.readfile(f)) do
        if line:find("spring-boot", 1, true) or line:find("springframework", 1, true) then
          return true
        end
      end
    end
  end
  return false
end

-- ── Test-bundle filtering (java-test ships non-bundle jars) ───────────────────

local NON_BUNDLE = {
  "com.microsoft.java.test.runner-jar-with-dependencies.jar",
  "jacocoagent.jar",
}

---@param jars string[]
---@return string[]
function M.filter_test_bundles(jars)
  return vim.tbl_filter(function(jar)
    local base = jar:match("([^/\\]+)$") or jar
    for _, bad in ipairs(NON_BUNDLE) do
      if base == bad then return false end
    end
    return true
  end, jars)
end

-- ── Build-tool detection ──────────────────────────────────────────────────────

--- Pick the build tool for `root`.
---@param root string
---@return table|nil tool    { kind = "gradle"|"maven", exe = "./gradlew"|"gradle"|"./mvnw"|"mvn" }
---@return string?   reason  set when tool is nil; says what to fix
function M.detect_tool(root)
  local has_gradle = false
  for _, f in ipairs(GRADLE_BUILD_FILES) do
    if is_file(root .. "/" .. f) then has_gradle = true; break end
  end
  local has_maven = is_file(root .. "/" .. MAVEN_BUILD_FILE)

  if has_gradle then
    if is_exec_file(root .. "/gradlew") then return { kind = "gradle", exe = "./gradlew" } end
    if vim.fn.executable("gradle") == 1 then return { kind = "gradle", exe = "gradle" } end
    if not has_maven then
      if is_file(root .. "/gradlew") then
        return nil, "gradlew exists but is not executable\n  fix: chmod +x " .. root .. "/gradlew"
      end
      return nil, "Gradle build file found, but neither ./gradlew nor gradle is on PATH"
    end
  end

  if has_maven then
    if is_exec_file(root .. "/mvnw") then return { kind = "maven", exe = "./mvnw" } end
    if vim.fn.executable("mvn") == 1 then return { kind = "maven", exe = "mvn" } end
    return nil, "pom.xml found, but neither ./mvnw nor mvn is on PATH"
  end

  return nil, "no build file (build.gradle[.kts], settings.gradle[.kts] or pom.xml) in " .. root
end

-- ── Task translation ──────────────────────────────────────────────────────────
-- Callers speak Gradle. The Maven column is what each Gradle task becomes.

local GRADLE_GOALS = {
  test    = "test",
  build   = "build",
  clean   = "clean",
  run     = "run",
  bootRun = "bootRun",
  bootJar = "bootJar",
}

local MAVEN_GOALS = {
  test    = "test",
  build   = "package",       -- Maven has no `build`; package is the closest
  clean   = "clean",
  run     = "exec:java",     -- needs exec-maven-plugin configured with mainClass
  bootRun = "spring-boot:run",
  bootJar = "package",       -- spring-boot repackage is bound to package
}

local GOALS = { gradle = GRADLE_GOALS, maven = MAVEN_GOALS }

--- Shell command for `task` in `root`, cd'd into the root first.
---@param root   string
---@param task   string  test | build | clean | run | bootRun | bootJar (Gradle names)
---@param filter string? test-name filter, honoured for `test` only
---@return string|nil cmd
---@return string|nil reason  set when cmd is nil
function M.command(root, task, filter)
  local tool, why = M.detect_tool(root)
  if not tool then return nil, why end

  local goal = GOALS[tool.kind][task]
  if not goal then
    return nil, ("'%s' has no %s equivalent"):format(task, tool.kind)
  end

  local cmd = "cd " .. vim.fn.shellescape(root) .. " && " .. tool.exe .. " " .. goal
  if task == "test" and filter and filter ~= "" then
    if tool.kind == "gradle" then
      cmd = cmd .. " --tests " .. vim.fn.shellescape("*." .. filter)
    else
      -- surefire filter: -Dtest=Class#method, here with a wildcard class
      cmd = cmd .. " -Dtest=" .. vim.fn.shellescape("*#" .. filter)
        .. " -Dsurefire.failIfNoSpecifiedTests=false"
    end
  end
  return cmd
end

--- Self-check: every Gradle task the callers use must have a Maven mapping.
---@return string[] issues  empty when consistent
function M.validate()
  local issues = {}
  for task in pairs(GRADLE_GOALS) do
    if not MAVEN_GOALS[task] then
      table.insert(issues, ("  '%s' has no Maven goal"):format(task))
    end
  end
  return issues
end

return M

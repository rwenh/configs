-- lua/core/util/jvm.lua — JVM helpers shared by the Java, Kotlin and runner specs
--

local M = {}

local GRADLE_BUILD_FILES = { "build.gradle", "build.gradle.kts", "settings.gradle", "settings.gradle.kts" }
local MAVEN_BUILD_FILE = "pom.xml"

local function is_file(p)
  return vim.fn.filereadable(p) == 1
end

local function is_exec_file(p)
  return is_file(p) and vim.fn.executable(p) == 1
end

-- ── Spring detection ──────────────────────────────────────────────────────────

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

function M.filter_test_bundles(jars)
  return vim.tbl_filter(function(jar)
    local base = jar:match("([^/\\]+)$") or jar
    for _, bad in ipairs(NON_BUNDLE) do
      if base == bad then
        return false
      end
    end
    return true
  end, jars)
end

-- ── Build-tool detection ──────────────────────────────────────────────────────

function M.detect_tool(root)
  local has_gradle = false
  for _, f in ipairs(GRADLE_BUILD_FILES) do
    if is_file(root .. "/" .. f) then
      has_gradle = true
      break
    end
  end
  local has_maven = is_file(root .. "/" .. MAVEN_BUILD_FILE)

  if has_gradle then
    if is_exec_file(root .. "/gradlew") then
      return { kind = "gradle", exe = "./gradlew" }
    end
    if vim.fn.executable("gradle") == 1 then
      return { kind = "gradle", exe = "gradle" }
    end
    if not has_maven then
      if is_file(root .. "/gradlew") then
        return nil, "gradlew exists but is not executable\n  fix: chmod +x " .. root .. "/gradlew"
      end
      return nil, "Gradle build file found, but neither ./gradlew nor gradle is on PATH"
    end
  end

  if has_maven then
    if is_exec_file(root .. "/mvnw") then
      return { kind = "maven", exe = "./mvnw" }
    end
    if vim.fn.executable("mvn") == 1 then
      return { kind = "maven", exe = "mvn" }
    end
    return nil, "pom.xml found, but neither ./mvnw nor mvn is on PATH"
  end

  return nil, "no build file (build.gradle[.kts], settings.gradle[.kts] or pom.xml) in " .. root
end

-- ── Task translation ──────────────────────────────────────────────────────────

local GRADLE_GOALS = {
  build = "build",
  clean = "clean",
  run = "run",
  bootRun = "bootRun",
  bootJar = "bootJar",
  test = "test",
}

local MAVEN_GOALS = {
  build = "package",
  clean = "clean",
  run = "exec:java",
  bootRun = "spring-boot:run",
  bootJar = "package",
  test = "test",
}

local GOALS = { gradle = GRADLE_GOALS, maven = MAVEN_GOALS }

function M.command(root, task)
  local tool, why = M.detect_tool(root)
  if not tool then
    return nil, why
  end

  local goal = GOALS[tool.kind][task]
  if not goal then
    return nil, ("'%s' has no %s equivalent"):format(task, tool.kind)
  end

  return "cd " .. vim.fn.shellescape(root) .. " && " .. tool.exe .. " " .. goal
end

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

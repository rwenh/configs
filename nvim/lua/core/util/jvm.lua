-- lua/core/util/jvm.lua — helpers shared by the Java and Kotlin specs
--
-- Owns: build-tool detection, logical task → Gradle/Maven goal mapping, Spring Boot detection,
-- and the test-bundle filter.
--
-- Why task mapping lives here: Gradle and Maven name the same work differently, and Maven has no
-- `build`, `bootRun` or `run` goal. `mvn build` fails with "unknown lifecycle phase". Callers ask for
-- a logical task ("spring-run") and get the goal for whichever tool the project actually uses.

local M = {}

---@alias JvmTask "build"|"test"|"run"|"spring-run"|"spring-package"

--- Logical task → goal, per build tool. A missing entry means the tool has no such task.
local GOALS = {
  gradle = {
    build = "build", test = "test", run = "run",
    ["spring-run"] = "bootRun", ["spring-package"] = "bootJar",
  },
  maven = {
    build = "package", test = "test", run = "exec:java",
    ["spring-run"] = "spring-boot:run", ["spring-package"] = "package",
  },
}

local function readable(p) return vim.fn.filereadable(p) == 1 end
local function runnable(p) return readable(p) and vim.fn.executable(p) == 1 end

--- Spring Boot is declared in the build file.
---@param root string?
---@return boolean
function M.is_spring_project(root)
  root = root or vim.fn.getcwd()
  for _, fname in ipairs({ "build.gradle", "build.gradle.kts", "pom.xml" }) do
    local f = root .. "/" .. fname
    if readable(f) then
      for _, line in ipairs(vim.fn.readfile(f)) do
        if line:find("spring-boot", 1, true) or line:find("springframework", 1, true) then
          return true
        end
      end
    end
  end
  return false
end

--- Build tool for `root`. Gradle wins when both are present, matching core.util.runner.
---@param root string
---@return "gradle"|"maven"|nil
function M.build_tool(root)
  if runnable(root .. "/gradlew") then return "gradle" end
  if (readable(root .. "/build.gradle") or readable(root .. "/build.gradle.kts")) and vim.fn.executable("gradle") == 1 then
    return "gradle"
  end
  if readable(root .. "/pom.xml") then return "maven" end
  return nil
end

--- Shell command for a logical task in `root`, using the wrapper when the project ships one.
---@param root string
---@param task JvmTask
---@return string|nil cmd
---@return string|nil err
function M.command(root, task)
  local tool = M.build_tool(root)
  if not tool then return nil, "no Gradle or Maven build file found in " .. root end
  local goal = GOALS[tool][task]
  if not goal then return nil, string.format("%s has no '%s' task", tool, task) end

  local wrapper = tool == "gradle" and "gradlew" or "mvnw"
  local exe
  if runnable(root .. "/" .. wrapper) then
    exe = "./" .. wrapper
  else
    exe = tool == "gradle" and "gradle" or "mvn"
  end
  return "cd " .. vim.fn.shellescape(root) .. " && " .. exe .. " " .. goal, nil
end

local NON_BUNDLE = {
  "com.microsoft.java.test.runner-jar-with-dependencies.jar",
  "jacocoagent.jar",
}

--- java-test ships helper jars (the runner, the JaCoCo agent) that must not be passed to jdtls as bundles.
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

return M

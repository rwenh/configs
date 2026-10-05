-- lua/core/util/jvm.lua — helpers shared by the Java and Kotlin specs

local M = {}

---@param root string?
---@return boolean
function M.is_spring_project(root)
  root = root or vim.fn.getcwd()
  for _, fname in ipairs({ "build.gradle", "build.gradle.kts", "pom.xml" }) do
    local f = root .. "/" .. fname
    if vim.fn.filereadable(f) == 1 then
      for _, line in ipairs(vim.fn.readfile(f)) do
        if line:find("spring-boot", 1, true) or line:find("springframework", 1, true) then
          return true
        end
      end
    end
  end
  return false
end

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

return M

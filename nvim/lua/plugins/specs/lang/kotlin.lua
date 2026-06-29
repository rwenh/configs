-- lua/plugins/specs/lang/kotlin.lua — Kotlin development
--

local shared = require("plugins.specs.lang.shared")

local SPRING_CACHE_TTL = (type(vim.g.kotlin_spring_cache_ttl) == "number")
  and vim.g.kotlin_spring_cache_ttl or 300   -- seconds; 0 = no cache

local _spring_cache = {}

vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("KotlinSpringCacheClear", { clear = true }),
  callback = function()
    -- Only invalidate stale entries; fresh ones survive directory changes.
    local now = os.time()
    for root, entry in pairs(_spring_cache) do
      if SPRING_CACHE_TTL == 0 or (now - entry.time) >= SPRING_CACHE_TTL then
        _spring_cache[root] = nil
      end
    end
  end,
  desc = "Expire stale Kotlin Spring Boot detection cache entries on directory change",
})

local function is_spring_project()
  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return false end

  -- Honour TTL=0 by skipping the cache entirely.
  if SPRING_CACHE_TTL > 0 then
    local entry = _spring_cache[root]
    if entry and (os.time() - entry.time) < SPRING_CACHE_TTL then
      return entry.result
    end
  end

  local result = false
  for _, fname in ipairs({ "build.gradle", "build.gradle.kts", "pom.xml" }) do
    local f = root .. "/" .. fname
    if vim.fn.filereadable(f) == 1 then
      local lines = vim.fn.readfile(f)
      for _, line in ipairs(lines) do
        if line:find("spring%-boot", 1, true) or line:find("springframework", 1, true) then
          result = true; break
        end
      end
    end
    if result then break end
  end

  _spring_cache[root] = { result = result, time = os.time() }
  return result
end

return {
  shared.treesitter({ "kotlin" }),

  {
    "akinsho/toggleterm.nvim",
    keys = (function()
      local function resolve()
        local ok_path,   path   = pcall(require, "core.util.path")
        local ok_runner, runner = pcall(require, "core.util.runner")
        if not ok_runner then vim.notify("[kotlin] core.util.runner unavailable", vim.log.levels.ERROR) end
        local root = (ok_path and path.find_root()) or vim.fn.getcwd()
        return root, ok_runner and runner or nil
      end

      local function mk_gradle_key(lhs, task, desc)
        return {
          lhs,
          function()
            local root, runner = resolve()
            if not runner then return end
            local cmd = runner.gradle_or_maven(root, task)
            if not cmd then
              vim.notify("[kotlin] No build tool found (gradlew/pom.xml)", vim.log.levels.WARN); return
            end
            require("core.util.term").float(cmd)
          end,
          desc = desc, ft = "kotlin",
        }
      end

      return {
        {
          "<leader>ktb",
          function()
            local root, runner = resolve()
            local cmd = runner and runner.gradle_or_maven(root, "build")
            if not cmd then
              if vim.fn.executable("kotlinc") ~= 1 then
                vim.notify("[kotlin] No build tool and kotlinc not in PATH", vim.log.levels.WARN); return
              end
              local file = vim.fn.expand("%:p")
              local jar  = vim.fn.expand("%:p:r") .. ".jar"
              cmd = "cd " .. vim.fn.shellescape(root)
                .. " && kotlinc " .. vim.fn.shellescape(file)
                .. " -include-runtime -d " .. vim.fn.shellescape(jar)
                .. " && java -jar " .. vim.fn.shellescape(jar)
            end
            require("core.util.term").float(cmd)
          end,
          desc = "Kotlin Build", ft = "kotlin",
        },
        mk_gradle_key("<leader>ktt", "test", "Kotlin Test"),
        mk_gradle_key("<leader>ktr", "run",  "Kotlin Run"),
        {
          "<leader>ktd",
          function()
            local ok, neogen = pcall(require, "neogen")
            if not ok then vim.notify("[kotlin] neogen not available", vim.log.levels.WARN); return end
            pcall(function() neogen.generate({ type = "func" }) end)
          end,
          desc = "Kotlin Generate KDoc", ft = "kotlin",
        },
        {
          "<leader>ktD",
          function()
            local ok, neogen = pcall(require, "neogen")
            if not ok then return end
            pcall(function() neogen.generate({ type = "class" }) end)
          end,
          desc = "Kotlin Generate KDoc (class)", ft = "kotlin",
        },
        {
          "<leader>kts",
          function()
            if not is_spring_project() then
              vim.notify("[kotlin] No Spring Boot project detected", vim.log.levels.WARN); return
            end
            local root, runner = resolve()
            local cmd = runner and runner.gradle_or_maven(root, "bootRun")
            if not cmd then vim.notify("[kotlin] bootRun requires Gradle or Maven", vim.log.levels.WARN); return end
            require("core.util.term").float(cmd, { close_on_exit = false })
          end,
          desc = "Kotlin Spring Boot Run (bootRun)", ft = "kotlin",
        },
        {
          "<leader>ktS",
          function()
            if not is_spring_project() then
              vim.notify("[kotlin] No Spring Boot project detected", vim.log.levels.WARN); return
            end
            local root, runner = resolve()
            local cmd = runner and runner.gradle_or_maven(root, "bootJar")
            if not cmd then return end
            require("core.util.term").float(cmd)
          end,
          desc = "Kotlin Spring Boot Package (bootJar)", ft = "kotlin",
        },
      }
    end)(),
  },
}

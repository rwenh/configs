-- lua/plugins/specs/lang/kotlin.lua — Kotlin development
--
-- LSP:    kotlin_language_server via lsp.lua
-- Format: ktlint via lsp.lua conform
-- Lint:   ktlint via lsp.lua nvim-lint
-- DAP:    java-debug-adapter via dap.lua (Kotlin runs on JVM)
-- Test:   neotest-java via test.lua; <leader>ktt here
--
-- Test mechanism guide:
--   <leader>ktt   → gradle/maven test (this file)
--   <leader>'n    → neotest-java (nearest test)
--   <leader>'t    → runner.lua (gradle_or_maven test)
--

local shared = require("plugins.specs.lang.shared")

return {
  shared.treesitter({ "kotlin" }),

  -- ── Build keymaps ──────────────────────────────────────────────────────────

  {
    "akinsho/toggleterm.nvim",
    keys = (function()

      -- Shared root + runner resolution — identical in all three handlers.
      local function resolve()
        local ok_path,   path   = pcall(require, "core.util.path")
        local ok_runner, runner = pcall(require, "core.util.runner")
        local root = (ok_path and path.find_root()) or vim.fn.getcwd()
        return root, ok_runner and runner or nil
      end

      -- Factory for test and run — pure gradle/maven, no fallback.
      local function mk_gradle_key(lhs, task, desc)
        return {
          lhs,
          function()
            local root, runner = resolve()
            if not runner then
              vim.notify("[kotlin] core.util.runner not available", vim.log.levels.WARN)
              return
            end
            local cmd = runner.gradle_or_maven(root, task)
            if not cmd then
              vim.notify("[kotlin] No build tool found (gradlew/pom.xml)",
                vim.log.levels.WARN)
              return
            end
            require("core.util.term").float(cmd)
          end,
          desc = desc,
          ft   = "kotlin",
        }
      end

      return {
        -- Build: gradle/maven with kotlinc fallback for single-file projects.
        {
          "<leader>ktb",
          function()
            local root, runner = resolve()
            local cmd = runner and runner.gradle_or_maven(root, "build")

            if not cmd then
              if vim.fn.executable("kotlinc") ~= 1 then
                vim.notify(
                  "[kotlin] No build tool found (gradlew/pom.xml) "
                  .. "and kotlinc not in PATH",
                  vim.log.levels.WARN
                )
                return
              end
              -- Kotlinc fallback: compile current file to a runnable jar.
              local file = vim.fn.expand("%:p")
              local jar  = vim.fn.expand("%:p:r") .. ".jar"
              cmd = "cd " .. vim.fn.shellescape(root)
                .. " && kotlinc " .. vim.fn.shellescape(file)
                .. " -include-runtime -d " .. vim.fn.shellescape(jar)
                .. " && java -jar " .. vim.fn.shellescape(jar)
            end

            require("core.util.term").float(cmd)
          end,
          desc = "Kotlin Build",
          ft   = "kotlin",
        },

        mk_gradle_key("<leader>ktt", "test", "Kotlin Test"),
        mk_gradle_key("<leader>ktr", "run",  "Kotlin Run"),
      }
    end)(),
  },
}

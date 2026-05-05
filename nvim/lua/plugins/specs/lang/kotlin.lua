-- lua/plugins/specs/lang/kotlin.lua — Kotlin development
--
-- LSP:    kotlin_language_server via lsp.lua
-- Format: ktlint via lsp.lua conform (canonical owner)
-- Lint:   ktlint via lsp.lua nvim-lint (canonical owner)
-- DAP:    java-debug-adapter via dap.lua (Kotlin runs on JVM)
-- Test:   neotest-java via test.lua; <leader>ktt here
--
-- Test mechanism guide:
--   <leader>ktt   → gradle/maven test (this file)
--   <leader>'n    → neotest-java (nearest test)
--   <leader>'t    → runner.lua (gradle_or_maven test)
--   For interactive TDD use neotest; for full builds use <leader>ktt.
--

local shared = require("plugins.specs.lang.shared")
return {
  shared.treesitter({ "kotlin" }),

  -- ── Build keymaps ──────────────────────────────────────────────────────────

  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>ktb",
        function()
          local ok_path, path   = pcall(require, "core.util.path")
          local ok_runner, runner = pcall(require, "core.util.runner")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local cmd
          if ok_runner then
            cmd = runner.gradle_or_maven(root, "build")
          end
          if not cmd then
            -- Fallback: compile a single .kt file if no build tool found.
            if vim.fn.executable("kotlinc") ~= 1 then
              vim.notify("[kotlin] No build tool found (gradlew/pom.xml) "
                .. "and kotlinc not in PATH", vim.log.levels.WARN)
              return
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
        desc = "Kotlin Build",
        ft   = "kotlin",
      },
      {
        "<leader>ktt",
        function()
          local ok_path, path     = pcall(require, "core.util.path")
          local ok_runner, runner = pcall(require, "core.util.runner")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          if not ok_runner then
            vim.notify("[kotlin] core.util.runner not available", vim.log.levels.WARN)
            return
          end
          local cmd = runner.gradle_or_maven(root, "test")
          if not cmd then
            vim.notify("[kotlin] No build tool found (gradlew/pom.xml)",
              vim.log.levels.WARN)
            return
          end
          require("core.util.term").float(cmd)
        end,
        desc = "Kotlin Test",
        ft   = "kotlin",
      },
      {
        "<leader>ktr",
        function()
          local ok_path, path     = pcall(require, "core.util.path")
          local ok_runner, runner = pcall(require, "core.util.runner")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          if not ok_runner then
            vim.notify("[kotlin] core.util.runner not available", vim.log.levels.WARN)
            return
          end
          local cmd = runner.gradle_or_maven(root, "run")
          if not cmd then
            vim.notify("[kotlin] No build tool found (gradlew/pom.xml)",
              vim.log.levels.WARN)
            return
          end
          require("core.util.term").float(cmd)
        end,
        desc = "Kotlin Run",
        ft   = "kotlin",
      },
    },
  },

  -- NOTE: ktlint formatter and linter are owned by lsp.lua.
  -- kotlin_language_server LSP is in lsp.lua servers table.
  -- Kotlin DAP configuration is in dap.lua (type="java", via java-debug-adapter).
  -- neotest-java adapter handles Kotlin JUnit tests (test.lua).
}

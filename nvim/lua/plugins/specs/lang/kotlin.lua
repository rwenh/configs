-- lua/plugins/specs/lang/kotlin.lua - Kotlin development

return {
  -- Treesitter parser
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "kotlin" })
      end
    end,
  },

  -- Conform: ktlint formatter
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        kotlin = { "ktlint" },
      },
    },
  },

  -- Lint: ktlint
  {
    "mfussenegger/nvim-lint",
    optional = true,
    -- FIX #6: optional=true config doesn't run — moved to init.
    -- Targeted assignment avoids overwriting lsp.lua's linter table.
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = "*.kt",
        once     = true,
        callback = function()
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.kotlin = { "ktlint" }
        end,
      })
    end,
  },

  -- NOTE (Fix #7): neotest-java adapter registration removed — test.lua
  -- already registers require("neotest-java")({ ignore_wrapper = false })
  -- centrally. Inserting it here too caused double registration and duplicate
  -- test results in Kotlin/Java buffers.

  -- Build integration via toggleterm
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>ktb",
        function()
          -- FIX #8: Use find_root() to locate project root regardless of cwd.
          -- filereadable("gradlew") resolved against cwd — if the user hasn't
          -- cd'd to the project root, detection silently fell back to the wrong
          -- build command.
          local root = require("core.util.path").find_root()
          local Terminal = require("toggleterm.terminal").Terminal
          local cmd = vim.fn.filereadable(root .. "/gradlew") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && ./gradlew build"
                   or vim.fn.filereadable(root .. "/pom.xml") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && mvn compile"
                   or "kotlinc *.kt -include-runtime -d app.jar"
          Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Build",
        ft   = "kotlin",
      },
      {
        "<leader>ktt",
        function()
          local root = require("core.util.path").find_root()
          local Terminal = require("toggleterm.terminal").Terminal
          local cmd = vim.fn.filereadable(root .. "/gradlew") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && ./gradlew test"
                   or vim.fn.filereadable(root .. "/pom.xml") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && mvn test"
                   or "echo 'No build tool found'"
          Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Test",
        ft   = "kotlin",
      },
      {
        "<leader>ktr",
        function()
          local root = require("core.util.path").find_root()
          local Terminal = require("toggleterm.terminal").Terminal
          local cmd = vim.fn.filereadable(root .. "/gradlew") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && ./gradlew run"
                   or "echo 'No Gradle wrapper found'"
          Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Run",
        ft   = "kotlin",
      },
    },
  },
}

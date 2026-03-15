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
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = vim.tbl_extend("force", lint.linters_by_ft or {}, {
        kotlin = { "ktlint" },
      })
    end,
  },

  -- Neotest adapter for Kotlin (JUnit via Gradle/Maven)
  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = { "rcasia/neotest-java" },
    opts = function(_, opts)
      opts.adapters = opts.adapters or {}
      table.insert(opts.adapters, require("neotest-java")({
        ignore_wrapper = false,
      }))
    end,
  },

  -- Build integration via toggleterm
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>ktb",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          -- Prefer Gradle, fall back to Maven
          local cmd = vim.fn.filereadable("gradlew") == 1 and "./gradlew build"
                   or vim.fn.filereadable("pom.xml") == 1 and "mvn compile"
                   or "kotlinc *.kt -include-runtime -d app.jar"
          Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Build",
        ft   = "kotlin",
      },
      {
        "<leader>ktt",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          local cmd = vim.fn.filereadable("gradlew") == 1 and "./gradlew test"
                   or vim.fn.filereadable("pom.xml") == 1 and "mvn test"
                   or "echo 'No build tool found'"
          Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Test",
        ft   = "kotlin",
      },
      {
        "<leader>ktr",
        function()
          local Terminal = require("toggleterm.terminal").Terminal
          local cmd = vim.fn.filereadable("gradlew") == 1 and "./gradlew run"
                   or "echo 'No Gradle wrapper found'"
          Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Run",
        ft   = "kotlin",
      },
    },
  },
}

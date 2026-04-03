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
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.kotlin = { "ktlint" }
    end,
  },

  -- Lint: ktlint
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = "*.kt",
        once     = true,
        group    = vim.api.nvim_create_augroup("KotlinLint", { clear = true }),
        callback = function()
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.kotlin = { "ktlint" }
        end,
      })
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
          -- RECALIBRATION: Safe toggleterm require and find_root
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local ok_path, path = pcall(require, "core.util.path")
          if not ok_path then
            vim.notify("path.lua not available", vim.log.levels.ERROR)
            return
          end

          local root = path.find_root()
          local cmd = vim.fn.filereadable(root .. "/gradlew") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && ./gradlew build"
                   or vim.fn.filereadable(root .. "/pom.xml") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && mvn compile"
                   or "kotlinc *.kt -include-runtime -d app.jar"

          term.Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Build",
        ft   = "kotlin",
      },
      {
        "<leader>ktt",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local ok_path, path = pcall(require, "core.util.path")
          if not ok_path then
            vim.notify("path.lua not available", vim.log.levels.ERROR)
            return
          end

          local root = path.find_root()
          local cmd = vim.fn.filereadable(root .. "/gradlew") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && ./gradlew test"
                   or vim.fn.filereadable(root .. "/pom.xml") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && mvn test"
                   or "echo 'No build tool found'"

          term.Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Test",
        ft   = "kotlin",
      },
      {
        "<leader>ktr",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end

          local ok_path, path = pcall(require, "core.util.path")
          if not ok_path then
            vim.notify("path.lua not available", vim.log.levels.ERROR)
            return
          end

          local root = path.find_root()
          local cmd = vim.fn.filereadable(root .. "/gradlew") == 1
                        and "cd " .. vim.fn.shellescape(root) .. " && ./gradlew run"
                   or "echo 'No Gradle wrapper found'"

          term.Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
        end,
        desc = "Kotlin Run",
        ft   = "kotlin",
      },
    },
  },
}

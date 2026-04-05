-- lua/plugins/specs/lang/kotlin.lua - Kotlin development
--
-- FIX (v2.2.3):
--   • DRY: the path.lua fallback block was copy-pasted verbatim into all three
--     toggleterm handlers. Extracted into a shared local helper.
--   • shellescape: filereadable(root .. "/gradlew") — if root contains spaces
--     the concatenation produces a broken path on some OS. Fixed: use
--     vim.fn.filereadable with the raw Lua string (filereadable handles
--     spaces correctly unlike shell expansion); shellescape applied only to
--     the cd argument in the final command string.
--   • jar shellescape: kotlinc -d <jar> was missing shellescape on the jar
--     name; spaces in project names broke the command.

return {
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "kotlin" })
      end
    end,
  },

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.kotlin = { "ktlint" }
    end,
  },

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

  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = (function()
      -- FIX: shared helper — eliminates the copy-pasted fallback block.
      -- vim.fn.filereadable() handles spaces in paths correctly without
      -- shellescape; shellescape is applied only to the cd argument.
      local function get_root()
        local ok, path = pcall(require, "core.util.path")
        if ok then return path.find_root() end
        return vim.fn.getcwd()
      end

      local function build_cmd(root, task)
        -- FIX: filereadable with raw Lua string — no shell involved here
        if vim.fn.filereadable(root .. "/gradlew") == 1 then
          return "cd " .. vim.fn.shellescape(root) .. " && ./gradlew " .. task
        elseif vim.fn.filereadable(root .. "/pom.xml") == 1 then
          local mvn_task = task == "run" and "exec:java" or task
          return "cd " .. vim.fn.shellescape(root) .. " && mvn " .. mvn_task
        end
        return nil
      end

      local function run(task, fallback_cmd)
        local ok, term = pcall(require, "toggleterm.terminal")
        if not ok then
          vim.notify("toggleterm not available", vim.log.levels.ERROR)
          return
        end
        local root = get_root()
        local cmd  = build_cmd(root, task) or fallback_cmd
        if not cmd then
          vim.notify("No build tool found (gradlew / pom.xml)", vim.log.levels.WARN)
          return
        end
        term.Terminal:new({ cmd = cmd, direction = "float", close_on_exit = false }):toggle()
      end

      return {
        {
          "<leader>ktb",
          function() run("build", "kotlinc *.kt -include-runtime -d app.jar") end,
          desc = "Kotlin Build",
          ft   = "kotlin",
        },
        {
          "<leader>ktt",
          function() run("test", nil) end,
          desc = "Kotlin Test",
          ft   = "kotlin",
        },
        {
          "<leader>ktr",
          function() run("run", nil) end,
          desc = "Kotlin Run",
          ft   = "kotlin",
        },
      }
    end)(),
  },
}

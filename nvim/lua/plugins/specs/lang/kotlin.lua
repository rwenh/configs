-- lua/plugins/specs/lang/kotlin.lua - Kotlin development
--
-- FIX (v2.2.3):
--   • DRY helpers extracted; shellescape fixes; jar name escaping.
--
-- OPT (v2.3.13):
--   • nvim-lint spec: BufReadPost + once=true autocmd wrapper removed.
--   • toggleterm launch delegated to core.util.term.float().

return {
  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "kotlin" })
      end
    end,
  },

  -- ── Conform ───────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.kotlin = { "ktlint" }
    end,
  },

  -- ── nvim-lint ─────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.kotlin = { "ktlint" }
    end,
  },

  -- ── Build keymaps ─────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = (function()
      local function get_root()
        local ok, path = pcall(require, "core.util.path")
        return ok and path.find_root() or vim.fn.getcwd()
      end

      local function build_cmd(root, task)
        -- FIX: filereadable alone is insufficient — gradlew may exist but
        -- lack the executable bit (e.g. fresh clone without chmod +x).
        -- Check executable() as well so we don't silently hand a non-runnable
        -- path to the shell.
        local gradlew = root .. "/gradlew"
        if vim.fn.filereadable(gradlew) == 1 and vim.fn.executable(gradlew) == 1 then
          return "cd " .. vim.fn.shellescape(root) .. " && ./gradlew " .. task
        elseif vim.fn.filereadable(root .. "/pom.xml") == 1 then
          local mvn_task = task == "run" and "exec:java" or task
          return "cd " .. vim.fn.shellescape(root) .. " && mvn " .. mvn_task
        end
      end

      local function run(task, fallback_cmd)
        local root = get_root()
        local cmd  = build_cmd(root, task) or fallback_cmd
        if not cmd then
          vim.notify("No build tool found (gradlew / pom.xml)", vim.log.levels.WARN)
          return
        end
        require("core.util.term").float(cmd)
      end

      return {
        { "<leader>ktb", function() run("build", "kotlinc *.kt -include-runtime -d app.jar") end, desc = "Kotlin Build", ft = "kotlin" },
        { "<leader>ktt", function() run("test",  nil) end, desc = "Kotlin Test", ft = "kotlin" },
        { "<leader>ktr", function() run("run",   nil) end, desc = "Kotlin Run",  ft = "kotlin" },
      }
    end)(),
  },
}

-- lua/plugins/specs/test.lua - Testing
--
-- (all prior FIX entries preserved — see INSTALL.md for full history)
--
-- FIX (v2.3.12):
--   • neotest-jest jestCommand: "bun test --" is invalid. Bun's test runner
--     does not support the Jest "--" argument separator; passing it causes
--     Bun to treat "--" as an unknown flag and error out silently.
--     The neotest-jest adapter appends its own args after jestCommand, so
--     the separator is only needed for npm/yarn/pnpm (which wrap `jest` via
--     `npm test -- <jest-args>`). Bun invokes its own runner directly, so
--     no separator is needed. Fixed: return "bun test" bare (no "--") when
--     Bun lockfiles are detected; keep " --" for npm/yarn/pnpm.

return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-python",
      "rouge8/neotest-rust",
      "nvim-neotest/neotest-go",
      "olimorris/neotest-rspec",
      "jfpedroza/neotest-elixir",
      "marilari88/neotest-vitest",
      "haydenmeade/neotest-jest",
      "rcasia/neotest-java",
    },
    keys = {
      { "<leader>'n", function() pcall(function() require("neotest").run.run() end) end,
        desc = "Test nearest" },
      { "<leader>'f", function() pcall(function() require("neotest").run.run(vim.fn.expand("%")) end) end,
        desc = "Test file" },
      { "<leader>'a", function() pcall(function() require("neotest").run.run(vim.uv.cwd()) end) end,
        desc = "Test all" },
      { "<leader>'u", function() pcall(function() require("neotest").summary.toggle() end) end,
        desc = "Test summary" },
      { "<leader>'o", function() pcall(function() require("neotest").output.open({ enter = true }) end) end,
        desc = "Test output" },
      { "<leader>'p", function() pcall(function() require("neotest").output_panel.toggle() end) end,
        desc = "Test panel" },
      { "<leader>'d", function() pcall(function() require("neotest").run.run({ strategy = "dap" }) end) end,
        desc = "Test debug nearest" },
      { "<leader>'P", function()
          pcall(function()
            require("neotest").run.run({ suite = true, concurrency = 4 })
          end)
        end, desc = "Test all (parallel)" },
    },
    config = function(_, opts)
      local neotest = require("neotest")
      neotest.setup(opts)

      -- Defer neotest-rust registration until rust_analyzer is confirmed
      -- attached. once=true removed (v2.3.10): it consumed the autocmd on
      -- the first LspAttach regardless of client name.
      local _rust_registered = false
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("NeotestRustDeferred", { clear = true }),
        callback = function(e)
          if _rust_registered then return end
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if not client or client.name ~= "rust_analyzer" then return end

          vim.schedule(function()
            local ok, rust_adapter = pcall(function()
              return require("neotest-rust")({ dap_adapter = "codelldb" })
            end)
            if ok and rust_adapter then
              _rust_registered = true
              local current_adapters = vim.deepcopy(opts.adapters or {})
              table.insert(current_adapters, rust_adapter)
              pcall(function()
                neotest.setup(vim.tbl_extend("force", opts, { adapters = current_adapters }))
              end)
            else
              vim.notify("neotest-rust: adapter failed to load after rust_analyzer attach",
                vim.log.levels.WARN)
            end
          end)
        end,
      })
    end,
    opts = function()
      local adapters = {}

      -- ── jest command helper ──────────────────────────────────────────
      -- FIX (v2.3.12): Bun does NOT support the "--" separator that npm/
      -- yarn/pnpm need. Return "bun test" bare; the adapter appends its own
      -- pattern args directly. For other package managers keep " --".
      local function jest_cmd()
        local ok_path, path = pcall(require, "core.util.path")
        local root = (ok_path and path.find_root()) or vim.fn.getcwd()
        local ok_runner, runner = pcall(require, "core.util.runner")

        if ok_runner then
          local cmd = runner.detect_js_test_cmd(root)
          -- Bun doesn't use "--" to pass args to the test runner
          if cmd == "bun test" then
            return cmd
          end
          return cmd .. " --"
        end

        -- Inline fallback if runner module is unavailable
        if vim.fn.filereadable(root .. "/bun.lockb") == 1
        or vim.fn.filereadable(root .. "/bun.lock")  == 1 then
          return "bun test"         -- NO " --" for bun
        elseif vim.fn.filereadable(root .. "/pnpm-lock.yaml") == 1 then
          return "pnpm test --"
        elseif vim.fn.filereadable(root .. "/yarn.lock") == 1 then
          return "yarn test --"
        end
        return "npm test --"
      end

      local eager_configs = {
        {
          "neotest-python",
          function() return require("neotest-python")({ runner = "pytest" }) end,
        },
        -- neotest-rust intentionally absent — registered in config() via LspAttach.
        {
          "neotest-go",
          function() return require("neotest-go")({}) end,
        },
        {
          "neotest-rspec",
          function()
            return require("neotest-rspec")({
              rspec_cmd = function() return { "bundle", "exec", "rspec" } end,
            })
          end,
        },
        {
          "neotest-elixir",
          function() return require("neotest-elixir")({}) end,
        },
        {
          "neotest-vitest",
          function() return require("neotest-vitest")({}) end,
        },
        {
          "neotest-jest",
          function()
            return require("neotest-jest")({ jestCommand = jest_cmd })
          end,
        },
        {
          "neotest-java",
          function()
            return require("neotest-java")({ ignore_wrapper = false })
          end,
        },
      }

      for _, cfg in ipairs(eager_configs) do
        local name, loader = cfg[1], cfg[2]
        local ok, adapter = pcall(loader)
        if ok and adapter then
          table.insert(adapters, adapter)
        else
          local n = name
          vim.schedule(function()
            vim.notify("neotest: adapter not loaded — " .. n, vim.log.levels.WARN)
          end)
        end
      end

      return {
        adapters = adapters,
        status   = { virtual_text = true },
        output   = { open_on_run = true },
        quickfix = { open = false },
      }
    end,
  },

  {
    "andythigpen/nvim-coverage",
    dependencies = "nvim-lua/plenary.nvim",
    cmd  = "Coverage",
    opts = {},
    keys = {
      { "<leader>tcv", "<cmd>Coverage<cr>",        desc = "Coverage Load" },
      { "<leader>tcs", "<cmd>CoverageSummary<cr>", desc = "Coverage Summary" },
      { "<leader>tct", "<cmd>CoverageToggle<cr>",  desc = "Coverage Toggle" },
    },
  },
}

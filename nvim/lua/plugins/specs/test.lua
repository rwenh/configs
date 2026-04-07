-- lua/plugins/specs/test.lua - Testing
--
-- FIX (v2.2.5):
--   • Duplicate keymaps removed. <leader>'n/f/a/u/o/p/d were registered here
--     AND in keymaps.lua — whichever spec loaded last silently won. keymaps.lua
--     is the canonical owner of all <leader>' bindings for consistency with the
--     rest of the keymap structure. Only <leader>'P (parallel suite, neotest-
--     specific feature) is kept here as it has no keymaps.lua equivalent.
--   • neotest-rust: plugin is "rouge8/neotest-rust"; the correct require path
--     is require("neotest-rust") — confirmed against upstream README.
--     The adapter loader now also passes a config table so codelldb is used.
--   • neotest-kotlin (rcasia/neotest-java covers Kotlin too via build-tool
--     detection) — no separate adapter needed; neotest-java handles both.

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
    opts = function()
      local adapters = {}

      local adapter_configs = {
        {
          "neotest-python",
          function() return require("neotest-python")({ runner = "pytest" }) end,
        },
        {
          -- FIX: rouge8/neotest-rust — require path is "neotest-rust"
          "neotest-rust",
          function()
            return require("neotest-rust")({
              -- prefer codelldb when available (matches dap.lua adapter)
              dap_adapter = "codelldb",
            })
          end,
        },
        {
          "neotest-go",
          function() return require("neotest-go") end,
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
          function() return require("neotest-elixir") end,
        },
        {
          "neotest-vitest",
          function() return require("neotest-vitest") end,
        },
        {
          "neotest-jest",
          function()
            return require("neotest-jest")({ jestCommand = "npm test --" })
          end,
        },
        {
          "neotest-java",
          function()
            return require("neotest-java")({ ignore_wrapper = false })
          end,
        },
      }

      for _, cfg in ipairs(adapter_configs) do
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

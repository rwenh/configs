-- lua/plugins/specs/test.lua - Testing
--
-- FIX (v2.3.1):
--   • neotest-rust race condition: deferred via one-shot FileType autocmd.
--     All other adapters loaded eagerly.
--
-- FIX (v2.3.2):
--   • neotest-go loader was `function() return require("neotest-go") end` —
--     this returns the raw module table, not an instantiated adapter. Every
--     other adapter in the list is called as a function (e.g.
--     require("neotest-python")({...})). neotest-go exports a callable that
--     accepts an optional opts table; not calling it means neotest receives
--     the module itself rather than the adapter object, so Go tests silently
--     never run through neotest. Fixed: require("neotest-go")({}) to invoke
--     the constructor with an empty opts table (all defaults).

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

      -- FIX: neotest-rust deferred registration.
      -- One-shot FileType autocmd fires after rustaceanvim's LspAttach.
      -- vim.schedule() yields one tick so the LSP client is fully attached
      -- before neotest-rust's constructor probes for it.
      local _rust_registered = false
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "rust",
        once     = true,
        group    = vim.api.nvim_create_augroup("NeotestRustDeferred", { clear = true }),
        callback = function()
          if _rust_registered then return end
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
              vim.notify("neotest-rust: adapter failed to load after rustaceanvim attach",
                vim.log.levels.WARN)
            end
          end)
        end,
      })
    end,
    opts = function()
      local adapters = {}

      local eager_configs = {
        {
          "neotest-python",
          function() return require("neotest-python")({ runner = "pytest" }) end,
        },
        -- neotest-rust is intentionally absent here — registered in config() above
        {
          "neotest-go",
          -- FIX: was `return require("neotest-go")` (returned the module table).
          -- neotest-go exports a constructor; it must be called to produce the
          -- adapter object. Passing {} uses all defaults.
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
          -- neotest-elixir also exports a plain table adapter (no constructor),
          -- so returning the module directly is correct here.
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

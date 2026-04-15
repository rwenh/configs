-- lua/plugins/specs/test.lua - Testing
--
-- FIX (v2.3.1):
--   • neotest-rust race condition: deferred via one-shot FileType autocmd.
--
-- FIX (v2.3.2):
--   • neotest-go: require("neotest-go")({}) constructor call.
--
-- FIX (v2.3.5):
--   • neotest-elixir: require("neotest-elixir")({}) constructor call.
--
-- FIX (v2.3.8):
--   • neotest-vitest: require("neotest-vitest")({}) constructor call.
--
-- FIX (v2.3.9b):
--   • neotest-rust deferred on LspAttach (client.name == "rust_analyzer")
--     instead of FileType. The FileType event fires when Neovim sets the
--     filetype — before rustaceanvim's LspAttach. vim.schedule() after
--     FileType only yields one tick, which is not enough time for rustaceanvim
--     to fully attach its client. neotest-rust probes for the rust-analyzer
--     client during its constructor; if the client isn't there yet the adapter
--     silently returns an invalid object and Rust tests never run through
--     neotest. Switching to LspAttach filtered by client name guarantees the
--     client is attached and ready before the constructor runs.
--
-- FIX (v2.3.10):
--   • once=true removed from the LspAttach autocmd that defers neotest-rust.
--     once=true fires and permanently removes the autocmd on the FIRST
--     LspAttach event regardless of client name. If any non-rust LSP (e.g.
--     lua_ls, basedpyright) attaches before rust_analyzer, the autocmd
--     fires, the `client.name ~= "rust_analyzer"` guard returns early, the
--     autocmd is gone, and neotest-rust is never registered for the rest of
--     the session. The _rust_registered boolean flag already provides the
--     idempotency guarantee that once=true was intended to give, so once=true
--     is redundant and harmful. Removed; the flag alone is sufficient.

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
      -- attached and ready. LspAttach guarantees the client exists; the
      -- _rust_registered flag prevents double-setup on subsequent attaches.
      --
      -- FIX (v2.3.10): once=true removed. With once=true the autocmd is
      -- consumed by the first LspAttach event regardless of which client
      -- fires it. Any non-rust LSP (lua_ls, basedpyright, clangd, …) that
      -- attaches before rust_analyzer silently discards the autocmd and
      -- neotest-rust is never registered for the session. The
      -- _rust_registered boolean already ensures the setup body runs only
      -- once, making once=true both redundant and harmful.
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

      local eager_configs = {
        {
          "neotest-python",
          function() return require("neotest-python")({ runner = "pytest" }) end,
        },
        -- neotest-rust intentionally absent here — registered in config() above
        -- via LspAttach to guarantee rust_analyzer is ready.
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

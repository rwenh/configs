-- lua/plugins/specs/test.lua — neotest + coverage
--

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
      { "<leader>'n", function()
          pcall(function() require("neotest").run.run() end)
        end, desc = "Test nearest" },
      { "<leader>'f", function()
          pcall(function() require("neotest").run.run(vim.fn.expand("%")) end)
        end, desc = "Test file" },
      { "<leader>'a", function()
          pcall(function() require("neotest").run.run(vim.uv.cwd()) end)
        end, desc = "Test all" },
      { "<leader>'u", function()
          pcall(function() require("neotest").summary.toggle() end)
        end, desc = "Test summary" },
      { "<leader>'o", function()
          pcall(function() require("neotest").output.open({ enter = true }) end)
        end, desc = "Test output" },
      { "<leader>'p", function()
          pcall(function() require("neotest").output_panel.toggle() end)
        end, desc = "Test panel" },
      { "<leader>'d", function()
          pcall(function() require("neotest").run.run({ strategy = "dap" }) end)
        end, desc = "Test debug nearest" },
      { "<leader>'P", function()
          local concurrency = vim.g.neotest_concurrency or 4
          pcall(function()
            require("neotest").run.run({ suite = true, concurrency = concurrency })
          end)
        end, desc = "Test all (parallel)" },
      { "<leader>'w", function()
          pcall(function() require("neotest").watch.toggle(vim.fn.expand("%")) end)
        end, desc = "Neotest watch file" },
      { "<leader>'W", function()
          pcall(function() require("neotest").watch.toggle() end)
        end, desc = "Neotest watch nearest" },
    },

    config = function(_, opts)
      local neotest = require("neotest")

      neotest.setup(opts)

      local _rust_registered = false
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("NeotestRustDeferred", { clear = true }),
        callback = function(e)
          if _rust_registered then return end
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if not client or client.name ~= "rust-analyzer" then return end

          vim.schedule(function()
            local ok, rust_adapter = pcall(function()
              return require("neotest-rust")({ dap_adapter = "codelldb" })
            end)

            if not ok or not rust_adapter then
              vim.notify("[neotest] rust adapter failed to load after rust-analyzer attach",
                vim.log.levels.WARN)
              return
            end

            _rust_registered = true

            local added = false
            pcall(function()
              if type(neotest.adapters) == "table"
              and type(neotest.adapters.add) == "function" then
                neotest.adapters.add(rust_adapter)
                added = true
              end
            end)

            if not added then
              local base = vim.deepcopy(opts.adapters or {})
              table.insert(base, rust_adapter)
              pcall(function()
                neotest.setup(vim.tbl_extend("force", opts, { adapters = base }))
              end)
            end
          end)
        end,
      })
    end,

    opts = function()
      -- ── jest_cmd helper ────────────────────────────────────────────────────

      local function jest_cmd()
        local ok_path,   path   = pcall(require, "core.util.path")
        local ok_runner, runner = pcall(require, "core.util.runner")
        if not ok_runner then
          vim.notify("[test] core.util.runner unavailable — jest adapter may not work",
            vim.log.levels.WARN)
          return "npm test --"
        end
        local buf_dir = vim.fn.expand("%:p:h")
        local root    = (ok_path and path.find_root(buf_dir)) or vim.fn.getcwd()
        local cmd     = runner.detect_js_test_cmd(root)
        return cmd == "bun test" and cmd or (cmd .. " --")
      end

      -- ── load_adapter helper ────────────────────────────────────────────────

      local adapters = {}
      local function load_adapter(name, loader)
        local ok, adapter = pcall(loader)
        if ok and adapter then
          table.insert(adapters, adapter)
        else
          vim.notify(
            "[neotest] adapter not loaded: " .. name
              .. "\nRun :Lazy install to ensure the adapter plugin is installed.",
            vim.log.levels.WARN
          )
        end
      end

      -- ── Eager adapter registration (all except rust) ───────────────────────
      load_adapter("neotest-python", function()
        return require("neotest-python")({ runner = "pytest" })
      end)

      -- neotest-rust registered in config() via LspAttach — not here.

      load_adapter("neotest-go", function()
        return require("neotest-go")({})
      end)

      load_adapter("neotest-rspec", function()
        local use_bundle = vim.fn.executable("bundle") == 1
        return require("neotest-rspec")({
          rspec_cmd = function()
            return use_bundle
              and { "bundle", "exec", "rspec" }
              or  { "rspec" }
          end,
        })
      end)

      load_adapter("neotest-elixir", function()
        return require("neotest-elixir")({})
      end)

      load_adapter("neotest-vitest", function()
        return require("neotest-vitest")({})
      end)

      load_adapter("neotest-jest", function()
        return require("neotest-jest")({ jestCommand = jest_cmd })
      end)

      load_adapter("neotest-java", function()
        return require("neotest-java")({ ignore_wrapper = false })
      end)

      return {
        adapters = adapters,
        status   = { virtual_text = true },
        output   = { open_on_run  = true  },
        quickfix = { open         = false },
      }
    end,
  },

  {
    "andythigpen/nvim-coverage",
    dependencies = "nvim-lua/plenary.nvim",
    cmd  = "Coverage",
    opts = {},
    keys = {
      { "<leader>tcv", "<cmd>Coverage<cr>",        desc = "Coverage Load"    },
      { "<leader>tcs", "<cmd>CoverageSummary<cr>", desc = "Coverage Summary" },
      { "<leader>tct", "<cmd>CoverageToggle<cr>",  desc = "Coverage Toggle"  },
    },
  },
}

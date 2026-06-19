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
      { "<leader>'n", function() pcall(function() require("neotest").run.run() end) end,                         desc = "Test nearest"        },
      { "<leader>'f", function() pcall(function() require("neotest").run.run(vim.fn.expand("%")) end) end,        desc = "Test file"           },
      { "<leader>'a", function() pcall(function() require("neotest").run.run(vim.uv.cwd()) end) end,              desc = "Test all"            },
      { "<leader>'u", function() pcall(function() require("neotest").summary.toggle() end) end,                   desc = "Test summary"        },
      { "<leader>'o", function() pcall(function() require("neotest").output.open({ enter = true }) end) end,      desc = "Test output"         },
      { "<leader>'p", function() pcall(function() require("neotest").output_panel.toggle() end) end,              desc = "Test panel"          },
      { "<leader>'d", function() pcall(function() require("neotest").run.run({ strategy = "dap" }) end) end,      desc = "Test debug nearest"  },
      { "<leader>'P", function()
          local concurrency = vim.g.neotest_concurrency or 4
          pcall(function() require("neotest").run.run({ suite = true, concurrency = concurrency }) end)
        end, desc = "Test all (parallel)" },
      { "<leader>'w", function() pcall(function() require("neotest").watch.toggle(vim.fn.expand("%")) end) end,   desc = "Neotest watch file"  },
      { "<leader>'W", function()
          pcall(function()
            local nt = require("neotest")
            nt.watch.stop()
            nt.watch.toggle(vim.uv.cwd())
          end)
        end, desc = "Neotest watch suite" },
      { "<leader>'cv", function() pcall(function() require("coverage").load(true) end) end,  desc = "Coverage load + show"       },
      { "<leader>'cs", function() pcall(function() require("coverage").summary() end) end,   desc = "Coverage summary"           },
      { "<leader>'ct", function() pcall(function() require("coverage").toggle() end) end,    desc = "Coverage toggle highlights" },
    },

    config = function(_, opts)
      local ok, err = pcall(function() require("neotest").setup(opts) end)
      if not ok then
        vim.notify("[neotest] setup failed: " .. tostring(err) .. "\nRun :checkhealth neotest", vim.log.levels.ERROR)
      end
    end,

    opts = function()
      local function jest_cmd()
        local ok_path,   path   = pcall(require, "core.util.path")
        local ok_runner, runner = pcall(require, "core.util.runner")
        if not ok_runner then return "npm test --" end
        local root = (ok_path and path.find_root(vim.fn.expand("%:p:h"))) or vim.fn.getcwd()
        local cmd  = runner.detect_js_test_cmd(root)
        -- FIX (medium) #21: bun test does not use "--" separator.
        if cmd == "bun test" then return cmd end
        return cmd .. " --"
      end

      local VITEST_CONFIGS = { "vitest.config.ts","vitest.config.js","vitest.config.mts","vitest.config.mjs","vitest.config.cjs" }
      local _vitest_cache  = {}
      local function is_vitest_root(root)
        if _vitest_cache[root] ~= nil then return _vitest_cache[root] end
        for _, pat in ipairs(VITEST_CONFIGS) do
          if vim.fn.findfile(pat, root .. ";") ~= "" then _vitest_cache[root] = true; return true end
        end
        _vitest_cache[root] = false; return false
      end

      local function lazy_knows_plugin(plugin_name)
        -- Check whether lazy.nvim has the plugin in its spec registry.
        local ok, lazy_config = pcall(require, "lazy.core.config")
        if not ok then return false end
        local plugins = lazy_config.plugins or {}
        for _, spec in pairs(plugins) do
          if type(spec) == "table" then
            local n = spec.name or (type(spec[1]) == "string" and spec[1]:match("[^/]+$"))
            if n and n:lower():find(plugin_name:lower(), 1, true) then
              return true
            end
          end
        end
        return false
      end

      local adapters = {}
      local function load_adapter(name, loader)
        -- Probe 1: runtime path (works for eager-loaded plugins).
        local mod     = name:gsub("%-", "/")
        local in_rtp  = #vim.api.nvim_get_runtime_file("lua/" .. mod .. ".lua",       false) > 0
                     or #vim.api.nvim_get_runtime_file("lua/" .. mod .. "/init.lua",   false) > 0

        local in_lazy = not in_rtp and lazy_knows_plugin(name)

        if not in_rtp and not in_lazy then return end

        local ok, adapter = pcall(loader)
        if ok and adapter then
          table.insert(adapters, adapter)
        else
          vim.schedule(function()
            vim.notify("[neotest] adapter failed to load: " .. name, vim.log.levels.WARN)
          end)
        end
      end

      load_adapter("neotest-python",  function() return require("neotest-python")({ runner = "pytest" }) end)
      load_adapter("neotest-rust",    function() return require("neotest-rust")({ dap_adapter = "codelldb" }) end)
      load_adapter("neotest-go",      function() return require("neotest-go")({}) end)
      load_adapter("neotest-rspec",   function()
        local use_bundle = vim.fn.executable("bundle") == 1
        return require("neotest-rspec")({
          rspec_cmd = function() return use_bundle and { "bundle","exec","rspec" } or { "rspec" } end,
        })
      end)
      load_adapter("neotest-elixir",  function() return require("neotest-elixir")({}) end)
      load_adapter("neotest-vitest",  function() return require("neotest-vitest")({}) end)
      load_adapter("neotest-jest",    function()
        local jest = require("neotest-jest")({ jestCommand = jest_cmd })
        local orig = jest.is_test_file
        jest.is_test_file = function(file_path)
          local ok_p, p = pcall(require, "core.util.path")
          local root = (ok_p and p.find_root(vim.fn.fnamemodify(file_path, ":h"))) or vim.fn.getcwd()
          if is_vitest_root(root) then return false end
          return orig(file_path)
        end
        return jest
      end)
      load_adapter("neotest-java",    function() return require("neotest-java")({ ignore_wrapper = false }) end)

      return {
        adapters   = adapters,
        status     = { virtual_text = true },
        output     = { open_on_run = true },
        quickfix   = { open = false },
      }
    end,
  },

  {
    "andythigpen/nvim-coverage",
    dependencies = "nvim-lua/plenary.nvim",
    cmd  = "Coverage",
    opts = {},
    keys = {
      { "<leader>tcv", "<cmd>Coverage<cr>",        desc = "Coverage Load (alias)"    },
      { "<leader>tcs", "<cmd>CoverageSummary<cr>", desc = "Coverage Summary (alias)" },
      { "<leader>tct", "<cmd>CoverageToggle<cr>",  desc = "Coverage Toggle (alias)"  },
    },
  },
}

-- ~/.config/nvim/init.lua — v2.5.0 entry point
--
-- Load order: bootstrap → options → autocmds → keymaps → commands → plugins → theme → highlights

-- 1. Bootstrap lazy.nvim + leader keys + version stamp (must be first)
require("core.bootstrap")

-- 2. Vim options (no plugins needed)
require("core.options")

-- 3. Autocommands
require("core.autocmds")

-- 4. Global keymaps
require("core.keymaps")

-- 5. User commands
require("core.commands")

-- 6. Plugin manager + all plugin specs
--
local _plugins_ok = true
local _plugins_err = nil

local ok, err = pcall(require, "plugins")
if not ok then
  _plugins_ok  = false
  _plugins_err = err
  vim.notify(
    "[init.lua] plugins/init.lua failed to load:\n" .. tostring(err)
      .. "\n\nTroubleshooting:\n"
      .. "  1. Delete ~/.local/share/nvim and ~/.cache/nvim\n"
      .. "  2. Restart Neovim — lazy.nvim will reinstall\n"
      .. "  3. Run :MasonInstallAll\n",
    vim.log.levels.ERROR
  )
end

-- 7. Theme (after plugins so colorscheme plugins are available;
local ok_theme, theme_err = pcall(function()
  require("core.theme").setup()
end)
if not ok_theme then
  vim.notify(
    "[init.lua] theme.setup() failed: " .. tostring(theme_err)
    .. "\nFalling back to built-in 'default' colorscheme.",
    vim.log.levels.WARN
  )
  pcall(vim.cmd.colorscheme, "default")
end

-- 8. Highlight overrides
local ok_hl, hl_err = pcall(function()
  require("core.highlights").apply()
end)
if not ok_hl then
  vim.notify(
    "[init.lua] highlights.apply() failed: " .. tostring(hl_err),
    vim.log.levels.WARN
  )
end

-- 9. Startup stats (only when plugins loaded successfully)
if _plugins_ok then
  vim.api.nvim_create_autocmd("User", {
    pattern  = "LazyDone",
    once     = true,
    callback = function()
      -- Cross-check lspconfig ↔ Mason naming drift (logs at DEBUG level only)
      pcall(function() require("core.util.packages").validate() end)
      pcall(function() require("core.util.packages").validate_dap() end)

      local typed_flags = {
        -- Core display & behaviour
        { "disable_highlight_overrides",   "boolean" },
        { "disable_tint",                  "boolean" },
        { "disable_smear_cursor",          "boolean" },
        { "disable_mini_pairs",            "boolean" },
        { "disable_autoformat",            "boolean" },
        { "disable_treesitter_folds",      "boolean" },
        { "auto_cd_root",                  "boolean" },
        { "runner_autosave",               "boolean" },
        -- LSP & formatting
        { "format_timeout_ms",             "number"  },
        { "format_timeout_by_ft",          "table"   },
        { "enable_pylint",                 "boolean" },
        { "lsp_on_attach_overrides",       "table"   },
        { "completion_sources_by_ft",      "table"   },
        { "debugpy_python",                "string"  },
        { "ts_import_preference",          "string"  },
        -- DAP & testing
        { "dap_bp_autosave_ms",            "number"  },
        { "neotest_concurrency",           "number"  },
        -- Task runner & workflow
        { "mason_install_timeout_ms",      "number"  },
        { "mason_extras",                  "table"   },
        { "workflow_template_debounce_ms", "number"  },
        { "overseer_auto_scroll",          "boolean" },
        -- Project root & paths
        { "path_max_walk_depth",           "number"  },
        { "path_cache_ttl",                "number"  },
        { "path_debug",                    "boolean" },
        { "path_ignore_dirs",              "table"   },
        { "cmake_build_dir",               "string"  },
        { "lazy_cache_path",               "string"  },
        -- UI extras
        { "symbol_usage_max_lines",        "number"  },
        { "force_mini_animate_scroll",     "boolean" },
        { "octo_timeout_ms",               "number"  },
        -- Language-specific
        { "disable_vsg_format",            "boolean" },
        { "elixir_use_nextls",             "boolean" },
        { "kotlin_spring_cache_ttl",       "number"  },
        { "python_venv_auto_refresh",      "boolean" },
        { "python_docstring_style",        "string"  },
        { "c_build_flags",                 "string"  },
        { "cpp_build_flags",               "string"  },
        { "fortran_build_flags",           "string"  },
        { "rustaceanvim_features",         "table"   },
        { "filetype_options",              "table"   },
        { "spell_wordlist",                "string"  },
        { "ts_disable",                    "table"   },
        { "ts_auto_install",               "boolean" },
        { "nextls_bin",                    "string"  },
      }
      local flag_issues = {}
      for _, entry in ipairs(typed_flags) do
        local key, expected = entry[1], entry[2]
        local val = vim.g[key]
        if val ~= nil and type(val) ~= expected then
          table.insert(flag_issues, string.format(
            "  vim.g.%-35s expected %-8s got %s",
            key, expected, type(val)
          ))
        end
      end
      if #flag_issues > 0 then
        vim.notify(
          "[init] Escape hatch type mismatch — these flags will silently use defaults:\n"
          .. table.concat(flag_issues, "\n"),
          vim.log.levels.WARN
        )
      end

      local ok_lazy, lazy = pcall(require, "lazy")
      if ok_lazy then
        local s = lazy.stats()
        vim.notify(
          string.format("⚡ %d plugins loaded in %.2fms", s.count, s.startuptime),
          vim.log.levels.INFO
        )
      end
    end,
  })
end

-- NOTE: vim.g.nvim_ide_version is set in core/bootstrap.lua (step 1).

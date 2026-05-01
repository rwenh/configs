-- lua/plugins/specs/lang/cpp.lua — C++ / CMake development
--
-- NOTE: clangd_extensions (inlay hints, AST) is owned by c.lua which has
--       ft = { "c", "cpp" } — C++ users get full clangd support from c.lua.
--       This file adds CMake build tooling only.
--

local CMAKE_FT = { "cpp", "cmake" }

return {
  -- ── cmake-tools ────────────────────────────────────────────────────────────

  {
    "Civitasv/cmake-tools.nvim",
    ft           = CMAKE_FT,
    dependencies = "nvim-lua/plenary.nvim",   -- used for cmake-tools async ops
    opts = {
      cmake_command         = "cmake",
      cmake_build_directory = vim.g.cmake_build_dir or "build",
      cmake_generate_options = { "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" },
    },
    config = function(_, opts)
      local ok = pcall(function() require("cmake-tools").setup(opts) end)
      if not ok then
        vim.notify("cmake-tools setup failed", vim.log.levels.WARN)
      end
    end,
    keys = (function()
      local entries = {
        { "<leader>ccg", "CMakeGenerate",    "CMake Generate"       },
        { "<leader>ccb", "CMakeBuild",       "CMake Build"          },
        { "<leader>ccr", "CMakeRun",         "CMake Run"            },
        { "<leader>cct", "CMakeRunTest",     "CMake Test"           },
        { "<leader>ccc", "CMakeClean",       "CMake Clean"          },
        { "<leader>ccs", "CMakeSelectTarget","CMake Select Target"  },
      }
      local keys = {}
      for _, e in ipairs(entries) do
        table.insert(keys, {
          e[1], "<cmd>" .. e[2] .. "<cr>",
          desc = e[3], ft = CMAKE_FT,
        })
      end
      return keys
    end)(),
  },

  -- ── Neogen docstrings ──────────────────────────────────────────────────────

  {
    "danymat/neogen",
    optional = true,
    ft       = { "cpp" },
    opts = {
      languages = {
        cpp = { template = { annotation_convention = "doxygen" } },
      },
    },
    keys = {
      {
        "<leader>ccd",
        function() pcall(function() require("neogen").generate() end) end,
        desc = "Generate Docstring (alias for <leader>xg)",
        ft   = { "cpp" },
      },
    },
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "cpp", "cmake" })
      end
    end,
  },
}

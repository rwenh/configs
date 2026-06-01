-- lua/plugins/specs/lang/cpp.lua — C++ / CMake development
--

local shared = require("plugins.specs.lang.shared")
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
      local ok, err = pcall(function() require("cmake-tools").setup(opts) end)
      if not ok then
        vim.notify(
          "[cpp] cmake-tools setup failed: " .. tostring(err)
          .. "\nRun :Lazy update cmake-tools.nvim",
          vim.log.levels.WARN
        )
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

  shared.treesitter({ "cpp", "cmake" }),
}

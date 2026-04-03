-- lua/plugins/specs/lang/cpp.lua - C++ development

return {
  {
    "Civitasv/cmake-tools.nvim",
    ft  = { "cpp", "cmake" },
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      cmake_command         = "cmake",
      cmake_build_directory = "build",
      cmake_generate_options = { "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" },
    },
    config = function(_, opts)
      local ok = pcall(function() require("cmake-tools").setup(opts) end)
      if not ok then
        vim.notify("cmake-tools setup failed", vim.log.levels.WARN)
      end
    end,
    keys = {
      { "<leader>ccg", "<cmd>CMakeGenerate<cr>",     desc = "CMake Generate",       ft = { "cpp", "cmake" } },
      { "<leader>ccb", "<cmd>CMakeBuild<cr>",        desc = "CMake Build",          ft = { "cpp", "cmake" } },
      { "<leader>ccr", "<cmd>CMakeRun<cr>",          desc = "CMake Run",            ft = { "cpp", "cmake" } },
      { "<leader>cct", "<cmd>CMakeRunTest<cr>",      desc = "CMake Test",           ft = { "cpp", "cmake" } },
      { "<leader>ccc", "<cmd>CMakeClean<cr>",        desc = "CMake Clean",          ft = { "cpp", "cmake" } },
      { "<leader>ccs", "<cmd>CMakeSelectTarget<cr>", desc = "CMake Select Target",  ft = { "cpp", "cmake" } },
    },
  },

  {
    "danymat/neogen",
    ft           = { "cpp", "c" },
    dependencies = "nvim-treesitter/nvim-treesitter",
    opts = {
      languages = {
        cpp = { template = { annotation_convention = "doxygen" } },
        c   = { template = { annotation_convention = "doxygen" } },
      },
    },
    config = function(_, opts)
      local ok = pcall(function() require("neogen").setup(opts) end)
      if not ok then
        vim.notify("neogen setup failed", vim.log.levels.WARN)
      end
    end,
    keys = {
      { "<leader>ccd", function() pcall(function() require("neogen").generate() end) end, desc = "Generate Docstring", ft = "cpp" },
      { "<leader>ccd", function() pcall(function() require("neogen").generate() end) end, desc = "Generate Docstring", ft = "c" },
    },
  },

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

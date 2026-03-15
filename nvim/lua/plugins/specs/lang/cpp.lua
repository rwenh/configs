-- lua/plugins/specs/lang/cpp.lua - C++ development
-- clangd_extensions (ft = {"c","cpp"}) lives in c.lua — no duplication needed.

return {
  -- CMake integration
  {
    "Civitasv/cmake-tools.nvim",
    ft  = { "cpp", "cmake" },
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      cmake_command         = "cmake",
      cmake_build_directory = "build",
      cmake_generate_options = { "-DCMAKE_EXPORT_COMPILE_COMMANDS=1" },
    },
    keys = {
      { "<leader>ccg", "<cmd>CMakeGenerate<cr>",   desc = "CMake Generate",   ft = "cpp" },
      { "<leader>ccb", "<cmd>CMakeBuild<cr>",      desc = "CMake Build",      ft = "cpp" },
      { "<leader>ccr", "<cmd>CMakeRun<cr>",        desc = "CMake Run",        ft = "cpp" },
      { "<leader>cct", "<cmd>CMakeRunTest<cr>",    desc = "CMake Test",       ft = "cpp" },
      { "<leader>ccc", "<cmd>CMakeClean<cr>",      desc = "CMake Clean",      ft = "cpp" },
      { "<leader>ccs", "<cmd>CMakeSelectTarget<cr>", desc = "CMake Select Target", ft = "cpp" },
    },
  },

  -- C++ snippets and docstring generation
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
    keys = {
      { "<leader>ccd", function() require("neogen").generate() end, desc = "Generate C++ Docstring", ft = "cpp" },
    },
  },

  -- Treesitter: cpp parser
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "cpp", "cmake" })
      end
    end,
  },

  -- Conform: clang-format for C++
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        cpp = { "clang_format" },
        c   = { "clang_format" },
      },
    },
  },
}

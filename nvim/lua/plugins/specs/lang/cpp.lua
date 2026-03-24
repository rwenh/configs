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
      { "<leader>ccg", "<cmd>CMakeGenerate<cr>",     desc = "CMake Generate",       ft = { "cpp", "cmake" } },
      { "<leader>ccb", "<cmd>CMakeBuild<cr>",        desc = "CMake Build",          ft = { "cpp", "cmake" } },
      { "<leader>ccr", "<cmd>CMakeRun<cr>",          desc = "CMake Run",            ft = { "cpp", "cmake" } },
      { "<leader>cct", "<cmd>CMakeRunTest<cr>",      desc = "CMake Test",           ft = { "cpp", "cmake" } },
      { "<leader>ccc", "<cmd>CMakeClean<cr>",        desc = "CMake Clean",          ft = { "cpp", "cmake" } },
      { "<leader>ccs", "<cmd>CMakeSelectTarget<cr>", desc = "CMake Select Target",  ft = { "cpp", "cmake" } },
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
      -- FIX #3: Added ft = "c" entry — plugin loads for both c and cpp but
      -- the original key only fired in cpp buffers, leaving C files without
      -- docstring generation access.
      { "<leader>ccd", function() require("neogen").generate() end, desc = "Generate Docstring", ft = "cpp" },
      { "<leader>ccd", function() require("neogen").generate() end, desc = "Generate Docstring", ft = "c" },
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

  -- NOTE (Fix #1 & #2): Conform clang-format spec removed from here.
  -- lsp.lua already registers c = { "clang-format" } and cpp = { "clang-format" }
  -- correctly (hyphen, not underscore). The original entry used "clang_format"
  -- (underscore) which conform does not recognise — formatting was silently
  -- skipped for all C/C++ files. Keeping it in lsp.lua as the single source.
}

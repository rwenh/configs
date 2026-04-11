-- lua/plugins/specs/lang/cpp.lua - C++ development
--
-- FIX (v2.3.1):
--   • neogen spec marked optional=true so it extends the primary spec in
--     advanced.lua rather than competing with it. With three non-optional
--     specs for the same plugin, the last one loaded silently dropped the
--     others' language opts. optional=true lets lazy merge opts tables.
--   • config() removed from the neogen spec — optional extension specs must
--     not call setup() themselves; the primary spec in advanced.lua owns
--     initialisation.
--   • Duplicate <leader>ccd key entry removed. The keys= table had two
--     identical entries (ft="cpp" and ft="c"). A single buffer-local keymap
--     via LspAttach or ft-scoped keys covers both; the second entry was a
--     no-op overwrite.

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

  -- FIX: optional=true — extends the primary neogen spec in advanced.lua.
  -- config() removed; advanced.lua's config() owns setup(). This spec only
  -- contributes language opts (doxygen for C/C++) and the <leader>ccd keymap.
  -- The duplicate ft="c" key entry has been collapsed into a single entry
  -- with a mode-agnostic ft list; ft filtering is handled by lazy at load time.
  {
    "danymat/neogen",
    optional = true,
    ft       = { "cpp", "c" },
    opts = {
      languages = {
        cpp = { template = { annotation_convention = "doxygen" } },
        c   = { template = { annotation_convention = "doxygen" } },
      },
    },
    keys = {
      {
        "<leader>ccd",
        function() pcall(function() require("neogen").generate() end) end,
        desc = "Generate Docstring",
        ft   = { "cpp", "c" },
      },
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

-- lua/plugins/specs/lang/cpp.lua — C++ / CMake development
--

local shared = require("plugins.specs.lang.shared")
local CMAKE_FT = { "cpp", "cmake" }

-- ── compile_commands.json auto-symlink ─────────────────────────────────────

vim.api.nvim_create_autocmd("FileType", {
  pattern  = "cpp",
  once     = true,
  group    = vim.api.nvim_create_augroup("CppCompileCommands", { clear = true }),
  callback = function()
    vim.schedule(function()
      shared.symlink_compile_commands("cpp")
    end)
  end,
  desc = "Auto-symlink compile_commands.json from build dir to project root",
})

-- ── cmake-tools ────────────────────────────────────────────────────────────

return {
  {
    "Civitasv/cmake-tools.nvim",
    ft           = CMAKE_FT,
    dependencies = "nvim-lua/plenary.nvim",
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
        { "<leader>ccg", "CMakeGenerate",     "CMake Generate"       },
        { "<leader>ccb", "CMakeBuild",        "CMake Build"          },
        { "<leader>ccr", "CMakeRun",          "CMake Run"            },
        { "<leader>cct", "CMakeRunTest",      "CMake Test"           },
        { "<leader>ccc", "CMakeClean",        "CMake Clean"          },
        { "<leader>ccs", "CMakeSelectTarget", "CMake Select Target"  },
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

  -- ── clangd: switch between header and implementation ──────────────────────
  --
  -- <leader>ch  — ClangdSwitchSourceHeader (built-in clangd command via LSP)

  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("ClangdSwitchHeader", { clear = true }),
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if not client or client.name ~= "clangd" then return end
          local ft = vim.bo[e.buf].filetype
          if not vim.tbl_contains({ "c", "cpp" }, ft) then return end
          vim.keymap.set("n", "<leader>ch", "<cmd>ClangdSwitchSourceHeader<cr>", {
            buffer = e.buf,
            silent = true,
            desc   = "C/C++ Switch header ↔ source (clangd)",
          })
        end,
        desc = "Register clangd switch-header keymap on attach",
      })
    end,
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

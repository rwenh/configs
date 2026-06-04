-- lua/plugins/specs/lang/c.lua — C language support
--

local shared = require("plugins.specs.lang.shared")

-- ── compile_commands.json detection ──────────────────────────────────────
-- Mirrors cpp.lua's approach for C projects without CMake.
local function try_symlink_compile_commands()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return end
  local dst = root .. "/compile_commands.json"
  if vim.fn.filereadable(dst) == 1 then return end
  local build_dir = vim.g.cmake_build_dir or "build"
  local candidates = {
    root .. "/" .. build_dir .. "/compile_commands.json",
    root .. "/build/Debug/compile_commands.json",
    root .. "/.build/compile_commands.json",
  }
  for _, src in ipairs(candidates) do
    if vim.fn.filereadable(src) == 1 then
      local ok = pcall(function() vim.fn.system({ "ln", "-sf", src, dst }) end)
      if ok and vim.fn.filereadable(dst) == 1 then
        vim.notify("[c] compile_commands.json linked from " .. vim.fn.fnamemodify(src, ":~:."), vim.log.levels.INFO)
      end
      return
    end
  end
end

vim.api.nvim_create_autocmd("FileType", {
  pattern  = "c",
  once     = true,
  group    = vim.api.nvim_create_augroup("CCompileCommands", { clear = true }),
  callback = function() vim.schedule(try_symlink_compile_commands) end,
  desc     = "Auto-symlink compile_commands.json for C projects",
})

return {
  -- ── clangd extensions ─────────────────────────────────────────────────────
  {
    "p00f/clangd_extensions.nvim",
    ft   = { "c", "cpp" },
    opts = { ast = { role_icons = require("core.util.icons").ast } },
    config = function(_, opts)
      local ok = pcall(function() require("clangd_extensions").setup(opts) end)
      if not ok then vim.notify("clangd_extensions setup failed", vim.log.levels.WARN); return end
      vim.api.nvim_create_autocmd("LspAttach", {
        group   = vim.api.nvim_create_augroup("ClangdInlayHints", { clear = true }),
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.name == "clangd" and client.server_capabilities.inlayHintProvider then
            pcall(function() vim.lsp.inlay_hint.enable(true, { bufnr = e.buf }) end)
          end
        end,
      })
    end,
  },

  shared.treesitter({ "c" }),

  -- ── Build + test keymaps ───────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>cb",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("gcc", "sudo zypper in gcc") then return end
          local file  = vim.fn.expand("%:p")
          local exe   = vim.fn.expand("%:p:r")
          local flags = vim.g.c_build_flags or "-Wall -Wextra -g"
          require("core.util.term").float(string.format(
            "gcc %s -o %s %s && %s", flags,
            vim.fn.shellescape(exe), vim.fn.shellescape(file), vim.fn.shellescape(exe)
          ))
        end,
        desc = "C Build & Run (gcc)", ft = "c",
      },
      {
        "<leader>cm",
        function() require("core.util.term").float_at_root("make") end,
        desc = "C Make", ft = "c",
      },
      {
        "<leader>csy",
        function()
          local exec = require("core.util.exec")
          if not exec.require_bin("gcc", "sudo zypper in gcc") then return end
          require("core.util.term").float(
            "gcc -Wall -Wextra -fsyntax-only " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "C Syntax Check", ft = "c",
      },
      -- ── CTest ─────────────────────────────────────────────────────────────
      {
        "<leader>ctt",
        function()
          if vim.fn.executable("ctest") ~= 1 then
            vim.notify("[c] ctest not found — install CMake", vim.log.levels.ERROR); return
          end
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local build_dir = vim.g.cmake_build_dir or "build"
          -- If no build dir yet, run cmake + build first.
          if vim.fn.isdirectory(root .. "/" .. build_dir) ~= 1 then
            vim.notify("[c] build/ not found — run <leader>ccb (CMake Build) first", vim.log.levels.WARN)
            return
          end
          require("core.util.term").float(
            "cd " .. vim.fn.shellescape(root)
            .. " && ctest --test-dir " .. build_dir .. " --output-on-failure"
          )
        end,
        desc = "C CTest Run", ft = "c",
      },
      {
        "<leader>ctb",
        function()
          if vim.fn.executable("ctest") ~= 1 then
            vim.notify("[c] ctest not found", vim.log.levels.ERROR); return
          end
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local build_dir = vim.g.cmake_build_dir or "build"
          -- cmake --build then ctest
          require("core.util.term").float(
            "cd " .. vim.fn.shellescape(root)
            .. " && cmake -B " .. build_dir .. " -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
            .. " && cmake --build " .. build_dir
            .. " && ctest --test-dir " .. build_dir .. " --output-on-failure"
          )
        end,
        desc = "C CMake Build + CTest", ft = "c",
      },
    },
  },

  -- ── Neogen docstrings ──────────────────────────────────────────────────────
  { "danymat/neogen", optional = true, ft = "c",
    opts = { languages = { c = { template = { annotation_convention = "doxygen" } } } } },

  -- ── LuaSnip snippets ───────────────────────────────────────────────────────
  {
    "L3MON4D3/LuaSnip", optional = true, ft = "c",
    config = function()
      require("core.util.snippets").load("c", function(s, t, i, _, ref)
        return {
          s("main", {
            t({ "#include <stdio.h>", "#include <stdlib.h>", "",
                "int main(int argc, char *argv[]) {", "    " }),
            i(0, "return 0;"), t({ "", "}" }),
          }),
          s("guard", {
            t("#ifndef "), i(1, "HEADER_H"),
            t({ "", "#define " }), ref(1, "HEADER_H"),
            t({ "", "", "" }), i(0),
            t({ "", "", "#endif /* " }), ref(1, "HEADER_H"), t(" */"),
          }),
          s("struct", {
            t("typedef struct {"), t({ "", "    " }), i(1, "int member"), t(";"),
            t({ "", "} " }), i(2, "MyStruct"), t(";"),
          }),
          s("for", {
            t("for (int "), i(1, "i"), t(" = 0; "), ref(1, "i"),
            t(" < "), i(2, "n"), t("; "), ref(1, "i"),
            t("++) {"), t({ "", "    " }), i(0), t({ "", "}" }),
          }),
          s("malloc", {
            i(1, "Type"), t(" *"), i(2, "ptr"), t(" = malloc(sizeof("),
            ref(1, "Type"), t(") * "), i(3, "n"), t(");"),
            t({ "", "if (!" }), ref(2, "ptr"),
            t({ ") {", '    fprintf(stderr, "malloc failed\\n");', "    return NULL;", "}" }),
          }),
        }
      end)
    end,
  },
}

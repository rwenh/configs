-- lua/plugins/specs/lang/c.lua - C development
--
-- FIX (v2.3.12):
--   • c.lua was a stub: only clangd_extensions. cpp.lua has CMake, conform,
--     build keymaps, docstring gen, snippets. C deserves feature parity.
--     Added:
--       • conform clang-format spec (was already in lsp.lua formatters_by_ft
--         but never declared in a lang spec for discoverability)
--       • nvim-lint: no standard C linter is in Mason that works without
--         project config, so clang-tidy is gated on executable presence
--       • toggleterm build keymaps: <leader>cb (gcc build+run), <leader>cm (make)
--       • LuaSnip: common C patterns (main, include guards, structs, loops)
--       • neogen optional=true for doxygen docstrings (cpp.lua already adds
--         this for "c" filetype; this spec ensures it loads on pure C projects)

return {
  -- ── clangd extensions (AST / inlay hints) ────────────────────────────
  {
    "p00f/clangd_extensions.nvim",
    ft   = { "c", "cpp" },
    opts = {
      ast = {
        role_icons = {
          type                  = "",
          declaration           = "",
          expression            = "",
          specifier             = "",
          statement             = "",
          ["template argument"] = "",
        },
      },
    },
    config = function(_, opts)
      local ok = pcall(function() require("clangd_extensions").setup(opts) end)
      if not ok then
        vim.notify("clangd_extensions setup failed", vim.log.levels.WARN)
        return
      end

      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("ClangdInlayHints", { clear = true }),
        pattern  = { "*.c", "*.cpp", "*.h", "*.hpp" },
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.name == "clangd" then
            pcall(function() vim.lsp.inlay_hint.enable(true, { bufnr = e.buf }) end)
          end
        end,
      })
    end,
  },

  -- ── conform: clang-format ────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      -- lsp.lua already sets c = { "clang-format" } but the lang spec
      -- makes it explicit and allows local override.
      opts.formatters_by_ft.c = { "clang-format" }
    end,
  },

  -- ── nvim-lint: clang-tidy (optional — requires compile_commands.json) ─
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.c", "*.h" },
        once     = true,
        group    = vim.api.nvim_create_augroup("CLint", { clear = true }),
        callback = function()
          if vim.fn.executable("clang-tidy") ~= 1 then return end
          local ok, lint = pcall(require, "lint")
          if not ok then return end
          lint.linters_by_ft.c = { "clangd" }
        end,
      })
    end,
  },

  -- ── Build keymaps via toggleterm ─────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      -- <leader>cb — compile & run current file with gcc
      {
        "<leader>cb",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end
          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")
          term.Terminal:new({
            cmd = string.format(
              "gcc -Wall -Wextra -g -o %s %s && %s",
              vim.fn.shellescape(exe),
              vim.fn.shellescape(file),
              vim.fn.shellescape(exe)
            ),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "C Build & Run (gcc)",
        ft   = "c",
      },
      -- <leader>cm — run make from project root
      {
        "<leader>cm",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          term.Terminal:new({
            cmd           = "cd " .. vim.fn.shellescape(root) .. " && make",
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "C Make",
        ft   = "c",
      },
      -- <leader>cc — syntax check only (no output binary)
      {
        "<leader>cc",
        function()
          local ok, term = pcall(require, "toggleterm.terminal")
          if not ok then
            vim.notify("toggleterm not available", vim.log.levels.ERROR)
            return
          end
          local file = vim.fn.expand("%:p")
          term.Terminal:new({
            cmd = string.format(
              "gcc -Wall -Wextra -fsyntax-only %s",
              vim.fn.shellescape(file)
            ),
            direction     = "float",
            close_on_exit = false,
          }):toggle()
        end,
        desc = "C Syntax Check",
        ft   = "c",
      },
    },
  },

  -- ── neogen: doxygen docstrings (extends advanced.lua primary spec) ────
  -- cpp.lua already adds ft="c" to its optional neogen spec; this entry is
  -- a no-op if cpp.lua is loaded. It exists so pure-C projects (no cpp.lua
  -- activation) still get <leader>xg working on .c files via ft trigger.
  {
    "danymat/neogen",
    optional = true,
    ft       = "c",
    opts = {
      languages = {
        c = { template = { annotation_convention = "doxygen" } },
      },
    },
  },

  -- ── LuaSnip: common C patterns ───────────────────────────────────────
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft = "c",
    config = function()
      local ok, ls = pcall(require, "luasnip")
      if not ok then return end

      local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node

      pcall(function()
        ls.add_snippets("c", {
          -- Full main() skeleton
          s("main", {
            t({ "#include <stdio.h>", "#include <stdlib.h>", "", "int main(int argc, char *argv[]) {", "    " }),
            i(0, "return 0;"),
            t({ "", "}" }),
          }),
          -- Include guard
          s("guard", {
            t("#ifndef "), i(1, "HEADER_H"),
            t({ "", "#define " }), f(function(args) return args[1][1] end, { 1 }),
            t({ "", "", "" }), i(0),
            t({ "", "", "#endif /* " }), f(function(args) return args[1][1] end, { 1 }),
            t(" */"),
          }),
          -- Struct definition
          s("struct", {
            t("typedef struct {"), t({ "", "    " }), i(1, "int member"),  t(";"),
            t({ "", "} " }), i(2, "MyStruct"), t(";"),
          }),
          -- For loop
          s("for", {
            t("for (int "), i(1, "i"), t(" = 0; "),
            f(function(args) return args[1][1] end, { 1 }),
            t(" < "), i(2, "n"), t("; "),
            f(function(args) return args[1][1] end, { 1 }),
            t("++) {"), t({ "", "    " }), i(0),
            t({ "", "}" }),
          }),
          -- Malloc + null check
          s("malloc", {
            i(1, "Type"), t(" *"), i(2, "ptr"), t(" = malloc(sizeof("),
            f(function(args) return args[1][1] end, { 1 }),
            t(") * "), i(3, "n"), t(");"),
            t({ "", "if (!" }), f(function(args) return args[2][1] end, { 2 }),
            t({ ") {", '    fprintf(stderr, "malloc failed\\n");', "    return NULL;", "}" }),
          }),
        })
      end)
    end,
  },

  -- ── Treesitter: ensure C parser installed ────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        -- "c" is already in treesitter.lua's ensure_installed list;
        -- this is a safety net for completeness, matching every other lang spec.
        vim.list_extend(opts.ensure_installed, { "c" })
      end
    end,
  },
}

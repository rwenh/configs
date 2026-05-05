-- lua/plugins/specs/lang/c.lua — C language support
--
-- NOTE: clangd_extensions covers both C and C++ (ft = { "c", "cpp" }).
--       C++ users: see cpp.lua for CMake tooling.
--

local shared = require("plugins.specs.lang.shared")
return {
  -- ── clangd extensions (AST / inlay hints) — covers C and C++ ─────────────

  {
    "p00f/clangd_extensions.nvim",
    ft   = { "c", "cpp" },
    opts = {
      ast = {
        role_icons = require("core.util.icons").ast,
      },
    },
    config = function(_, opts)
      local ok = pcall(function() require("clangd_extensions").setup(opts) end)
      if not ok then
        vim.notify("clangd_extensions setup failed", vim.log.levels.WARN)
        return
      end

      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("ClangdInlayHints", { clear = true }),
        pattern  = { "c", "cpp" },
        callback = function(e)
          local client = vim.lsp.get_client_by_id(
            (vim.lsp.get_clients({ bufnr = e.buf, name = "clangd" })[1] or {}).id
          )
          if client then
            pcall(function() vim.lsp.inlay_hint.enable(true, { bufnr = e.buf }) end)
          end
        end,
      })
    end,
  },

  -- ── Conform ────────────────────────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft     = opts.formatters_by_ft or {}
      opts.formatters_by_ft.c   = { "clang-format" }
    end,
  },

  -- ── nvim-lint ──────────────────────────────────────────────────────────────
  {
    "mfussenegger/nvim-lint",
    optional = true,
    init = function()
      if vim.fn.executable("clang-tidy") ~= 1 then return end
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft.c = { "clang-tidy" }
    end,
  },

  shared.treesitter({ "c" }),

  -- ── Build keymaps ──────────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    keys = {
      {
        "<leader>cb",
        function()
          if vim.fn.executable("gcc") ~= 1 then
            vim.notify("[c] gcc not found — install gcc to build C files",
              vim.log.levels.ERROR)
            return
          end
          local file  = vim.fn.expand("%:p")
          local exe   = vim.fn.expand("%:p:r")
          local flags = vim.g.c_build_flags or "-Wall -Wextra -g"
          require("core.util.term").float(string.format(
            "gcc %s -o %s %s && %s",
            flags,
            vim.fn.shellescape(exe),
            vim.fn.shellescape(file),
            vim.fn.shellescape(exe)
          ))
        end,
        desc = "C Build & Run (gcc)",
        ft   = "c",
      },
      {
        "<leader>cm",
        function() require("core.util.term").float_at_root("make") end,
        desc = "C Make",
        ft   = "c",
      },
      {
        "<leader>csy",
        function()
          if vim.fn.executable("gcc") ~= 1 then
            vim.notify("[c] gcc not found", vim.log.levels.ERROR)
            return
          end
          require("core.util.term").float(
            "gcc -Wall -Wextra -fsyntax-only "
            .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "C Syntax Check",
        ft   = "c",
      },
    },
  },

  -- ── Neogen docstrings ──────────────────────────────────────────────────────
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

  -- ── LuaSnip snippets ───────────────────────────────────────────────────────
  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft       = "c",
    config   = function()
      local ok, ls = pcall(require, "luasnip")
      if not ok then return end

      local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node

      local function ref(n, default)
        return f(function(args)
          return (args[n] and args[n][1] ~= "") and args[n][1] or (default or "")
        end, { n })
      end

      pcall(function()
        ls.add_snippets("c", {
          s("main", {
            t({ "#include <stdio.h>", "#include <stdlib.h>", "",
                "int main(int argc, char *argv[]) {", "    " }),
            i(0, "return 0;"),
            t({ "", "}" }),
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
            t({ ") {", '    fprintf(stderr, "malloc failed\\n");',
                "    return NULL;", "}" }),
          }),
        })
      end)
    end,
  },
}

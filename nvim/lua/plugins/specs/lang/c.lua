-- lua/plugins/specs/lang/c.lua - C development
--
-- FIX (v2.3.12): Added feature parity with cpp.lua — conform, lint, build
--   keymaps, LuaSnip patterns, neogen optional spec.
--
-- OPT (v2.3.13):
--   • Build keymaps use core.util.term.float() — 3 × boilerplate removed.
--   • nvim-lint: BufReadPost + once=true autocmd wrapper removed (see css.lua).
--   • BUG FIX: lint.linters_by_ft.c was set to { "clangd" } — clangd is an
--     LSP server, not an nvim-lint linter. Corrected to { "clang-tidy" }.
--     clang-tidy still requires compile_commands.json to be useful; the
--     executable guard is preserved.
--   • <leader>cc keymap renamed to <leader>csy (C SYntax check) to avoid
--     collision with the which-key <leader>cc group prefix owned by cpp/cmake.

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

  -- ── Conform: clang-format ─────────────────────────────────────────────
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}
      opts.formatters_by_ft.c = { "clang-format" }
    end,
  },

  -- ── nvim-lint: clang-tidy (requires compile_commands.json) ───────────
  -- OPT: direct init() — no autocmd wrapper (see css.lua note).
  -- BUG FIX: was { "clangd" } — corrected to { "clang-tidy" }.
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

  -- ── Build keymaps ─────────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    optional = true,
    keys = {
      {
        "<leader>cb",
        function()
          local file = vim.fn.expand("%:p")
          local exe  = vim.fn.expand("%:p:r")
          require("core.util.term").float(string.format(
            "gcc -Wall -Wextra -g -o %s %s && %s",
            vim.fn.shellescape(exe), vim.fn.shellescape(file), vim.fn.shellescape(exe)
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
      -- RENAMED from <leader>cc → <leader>csy to avoid collision with the
      -- which-key <leader>cc* group prefix owned by cpp.lua / CMake.
      {
        "<leader>csy",
        function()
          require("core.util.term").float(
            "gcc -Wall -Wextra -fsyntax-only " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "C Syntax Check",
        ft   = "c",
      },
    },
  },

  -- ── neogen: doxygen docstrings ────────────────────────────────────────
  -- No-op if cpp.lua is loaded (it also covers ft="c").
  -- Ensures pure-C projects get <leader>xg via ft trigger.
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

  -- ── LuaSnip: common C patterns ────────────────────────────────────────
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
          s("main", {
            t({ "#include <stdio.h>", "#include <stdlib.h>", "", "int main(int argc, char *argv[]) {", "    " }),
            i(0, "return 0;"),
            t({ "", "}" }),
          }),
          s("guard", {
            t("#ifndef "), i(1, "HEADER_H"),
            t({ "", "#define " }), f(function(args) return args[1][1] end, { 1 }),
            t({ "", "", "" }), i(0),
            t({ "", "", "#endif /* " }), f(function(args) return args[1][1] end, { 1 }),
            t(" */"),
          }),
          s("struct", {
            t("typedef struct {"), t({ "", "    " }), i(1, "int member"), t(";"),
            t({ "", "} " }), i(2, "MyStruct"), t(";"),
          }),
          s("for", {
            t("for (int "), i(1, "i"), t(" = 0; "),
            f(function(args) return args[1][1] end, { 1 }),
            t(" < "), i(2, "n"), t("; "),
            f(function(args) return args[1][1] end, { 1 }),
            t("++) {"), t({ "", "    " }), i(0),
            t({ "", "}" }),
          }),
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

  -- ── Treesitter ────────────────────────────────────────────────────────
  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "c" })
      end
    end,
  },
}

-- lua/plugins/specs/lang/c.lua — C language support
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

      vim.api.nvim_create_autocmd("LspAttach", {
        group   = vim.api.nvim_create_augroup("ClangdInlayHints", { clear = true }),
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if client and client.name == "clangd"
          and client.server_capabilities.inlayHintProvider then
            pcall(function() vim.lsp.inlay_hint.enable(true, { bufnr = e.buf }) end)
          end
        end,
        desc = "Auto-enable clangd inlay hints on attach",
      })
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
          local exec = require("core.util.exec")
          if not exec.require_bin("gcc", "sudo zypper in gcc") then return end
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
          local exec = require("core.util.exec")
          if not exec.require_bin("gcc", "sudo zypper in gcc") then return end
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
      require("core.util.snippets").load("c", function(s, t, i, _, ref)
        return {
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
        }
      end)
    end,
  },

}

-- lua/plugins/specs/lang/shared.lua — shared constants and spec helpers
--

local M = {}

-- ── Filetype lists ─────────────────────────────────────────────────────────

M.JS_FT    = { "javascript", "javascriptreact" }
M.TS_FT    = { "typescript", "typescriptreact" }
M.JS_TS_FT = { "javascript", "javascriptreact", "typescript", "typescriptreact" }

M.WEB_FT = {
  "html", "htmldjango", "jinja.html",
  "css", "scss", "less",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

M.TAILWIND_FT = {
  "html", "css",
  "javascript", "javascriptreact",
  "typescript", "typescriptreact",
  "vue", "svelte",
}

-- ── FT → LSP server name ──────────────────────────────────────────────────
M.ft_to_lsp = {
  lua             = "lua_ls",
  python          = "basedpyright",
  rust            = "rust-analyzer",   -- managed by rustaceanvim
  go              = "gopls",
  javascript      = "typescript-tools",
  typescript      = "typescript-tools",
  javascriptreact = "typescript-tools",
  typescriptreact = "typescript-tools",
  java            = "jdtls",           -- managed by nvim-jdtls
  kotlin          = "kotlin_language_server",
  ruby            = "solargraph",
  elixir          = "elixirls",
  c               = "clangd",
  cpp             = "clangd",
  html            = "html",
  css             = "cssls",
  scss            = "cssls",
  json            = "jsonls",
  yaml            = "yamlls",
  sql             = "sqls",
  fortran         = "fortls",
  zig             = "zls",
  vhdl            = "vhdl_ls",
  cobol           = "cobol_ls",
}

-- ── FT → formatter name(s) ────────────────────────────────────────────────
-- Lang specs can read this to display the active formatter or build guards.
M.ft_to_formatter = {
  lua             = { "stylua" },
  python          = { "black", "isort" },
  go              = { "goimports", "gofumpt" },
  rust            = { "rustfmt" },
  javascript      = { "prettier" },
  javascriptreact = { "prettier" },
  typescript      = { "prettier" },
  typescriptreact = { "prettier" },
  html            = { "prettier" },
  css             = { "prettier" },
  scss            = { "prettier" },
  less            = { "prettier" },
  json            = { "prettier" },
  yaml            = { "prettier" },
  markdown        = { "prettier" },
  sh              = { "shfmt" },
  ruby            = { "rubocop" },
  kotlin          = { "ktlint" },
  c               = { "clang-format" },
  cpp             = { "clang-format" },
  fortran         = { "fprettify" },
  zig             = { "zigfmt" },
  vhdl            = { "vsg" },
  elixir          = { "mix" },
}

-- ── Spec helpers ───────────────────────────────────────────────────────────

---@param parsers string[]
---@return table  lazy.nvim plugin spec
function M.treesitter(parsers)
  return {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        local seen = {}
        for _, p in ipairs(opts.ensure_installed) do seen[p] = true end
        for _, p in ipairs(parsers) do
          if not seen[p] then table.insert(opts.ensure_installed, p); seen[p] = true end
        end
      end
    end,
  }
end

-- Usage (from any lang spec):
--   shared.conform("mylang", { "myformatter" })
--
---@param ft         string    Neovim filetype
---@param formatters string[]  list of conform formatter names
function M.conform(ft, formatters)
  return {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters_by_ft        = opts.formatters_by_ft or {}
      opts.formatters_by_ft[ft]    = opts.formatters_by_ft[ft] or {}
      for _, fmt in ipairs(formatters) do
        if not vim.tbl_contains(opts.formatters_by_ft[ft], fmt) then
          table.insert(opts.formatters_by_ft[ft], fmt)
        end
      end
    end,
  }
end

-- Usage (from any lang spec):
--   shared.lint("mylang", { "mylinter" })
--
---@param ft      string    Neovim filetype
---@param linters string[]  list of nvim-lint linter names
function M.lint(ft, linters)
  return {
    "mfussenegger/nvim-lint",
    optional = true,
    config = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft    = lint.linters_by_ft or {}
      lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
      for _, l in ipairs(linters) do
        if not vim.tbl_contains(lint.linters_by_ft[ft], l) then
          table.insert(lint.linters_by_ft[ft], l)
        end
      end
    end,
  }
end

return M

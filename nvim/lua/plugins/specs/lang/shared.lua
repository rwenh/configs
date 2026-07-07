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
--
M.ft_to_lsp = {
  lua             = "lua_ls",
  python          = "basedpyright",
  -- NOTE: rust-analyzer is managed by rustaceanvim, NOT lspconfig.
  rust            = "rust-analyzer",
  go              = "gopls",
  javascript      = "typescript-tools",
  typescript      = "typescript-tools",
  javascriptreact = "typescript-tools",
  typescriptreact = "typescript-tools",
  -- NOTE: jdtls is managed by nvim-jdtls, NOT lspconfig.
  java            = "jdtls",
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

-- If M.lsp_managed_externally[ft] is true, skip lspconfig setup for that ft.
M.lsp_managed_externally = {
  rust = true,  -- managed by rustaceanvim
  java = true,  -- managed by nvim-jdtls
}

-- ── FT → formatter name(s) ────────────────────────────────────────────────

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
---@return table
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

---@param ft         string
---@param formatters string[]
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

---@param ft      string
---@param linters string[]
function M.lint(ft, linters)
  return {
    "mfussenegger/nvim-lint",
    optional = true,
    config = function()
      local ok, lint = pcall(require, "lint")
      if not ok then return end
      lint.linters_by_ft     = lint.linters_by_ft or {}
      lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
      for _, l in ipairs(linters) do
        if not vim.tbl_contains(lint.linters_by_ft[ft], l) then
          table.insert(lint.linters_by_ft[ft], l)
        end
      end
    end,
  }
end

-- ── symlink_compile_commands ───────────────────────────────────────────────
--
---@param prefix string
---@param extra  string[]?
function M.symlink_compile_commands(prefix, extra)
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return end

  local dst = root .. "/compile_commands.json"
  if vim.fn.filereadable(dst) == 1 or vim.fn.isdirectory(dst) == 1 then return end

  local build_dir = vim.g.cmake_build_dir or "build"
  local candidates = {
    root .. "/" .. build_dir .. "/compile_commands.json",
    root .. "/build/Debug/compile_commands.json",
    root .. "/build/Release/compile_commands.json",
    root .. "/.build/compile_commands.json",
  }

  if type(extra) == "table" then
    for _, c in ipairs(extra) do table.insert(candidates, c) end
  end

  for _, src in ipairs(candidates) do
    if vim.fn.filereadable(src) == 1 then
      if vim.fn.executable("ln") ~= 1 then
        vim.notify(
          "[" .. prefix .. "] compile_commands.json found at "
          .. vim.fn.fnamemodify(src, ":~:.")
          .. " but 'ln' is not on PATH — skipping symlink.\n"
          .. "Copy it to the project root manually, or install coreutils.",
          vim.log.levels.DEBUG
        )
        return
      end

      local ok = pcall(function() vim.fn.system({ "ln", "-sf", src, dst }) end)
      if ok and vim.fn.filereadable(dst) == 1 then
        vim.notify(
          "[" .. prefix .. "] compile_commands.json linked from "
          .. vim.fn.fnamemodify(src, ":~:."),
          vim.log.levels.INFO
        )
      else
        vim.notify(
          "[" .. prefix .. "] found " .. vim.fn.fnamemodify(src, ":~:.")
          .. " but failed to symlink to " .. vim.fn.fnamemodify(dst, ":~:.")
          .. " — check write permissions on the project root.",
          vim.log.levels.DEBUG
        )
      end
      return
    end
  end
end

return M

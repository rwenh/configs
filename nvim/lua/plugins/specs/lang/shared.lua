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

-- ── sanitize_build_flags ──────────────────────────────────────────────────────
--
---@param  flags string?  raw flag string, e.g. from vim.g.c_build_flags
---@return string sanitized
---@return boolean changed  true if any characters were actually stripped
function M.sanitize_build_flags(flags)
  return require("core.util.exec").sanitize_build_flags(flags)   -- allow-list; single implementation
end

function M.run_make()
  local dir = require("core.util.path").find_up({ "Makefile", "makefile", "GNUmakefile" })
  if not dir then
    vim.notify("[make] no Makefile found above the current file", vim.log.levels.WARN)
    return
  end
  local term = require("core.util.term")
  term.float(term.cd_prefix(dir) .. "make")
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

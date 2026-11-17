-- lua/plugins/specs/lang/css.lua — CSS, CSS modules and Tailwind
--

local lspreg = require("core.util.lspreg")
local shared = require("plugins.specs.lang.shared")

-- stylelint lints only projects that configure it.
shared.gate_linter({ "css", "scss", "less" }, "stylelint", {
  files = {
    ".stylelintrc",
    ".stylelintrc.json",
    ".stylelintrc.js",
    ".stylelintrc.cjs",
    ".stylelintrc.yaml",
    ".stylelintrc.yml",
    "stylelint.config.js",
    "stylelint.config.cjs",
  },
  pkg_key = "stylelint",
  bin = "stylelint",
})

-- ── CSS modules LSP ───────────────────────────────────────────────────────────
-- Optional: a missing binary is not reported, because completion for modules is a nicety.
local _cssmodules_started = false

local function enable_cssmodules_lsp()
  if _cssmodules_started then
    return
  end
  _cssmodules_started = true
  lspreg.register("cssmodules_ls", {
    cmd = { "cssmodules-language-server" },
    optional = true,
    init_options = { isCSSModules = true },
    filetypes = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
  })
end

vim.api.nvim_create_autocmd("BufReadPost", {
  pattern = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
  once = true,
  group = vim.api.nvim_create_augroup("CssModulesLspInit", { clear = true }),
  callback = function()
    vim.schedule(enable_cssmodules_lsp)
  end,
  desc = "Enable the cssmodules LSP on the first CSS/TSX/JSX buffer",
})

-- ── CSS custom-property jump ──────────────────────────────────────────────────
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("CssVarJump", { clear = true }),
  callback = function(e)
    local client = vim.lsp.get_client_by_id(e.data.client_id)
    if not client or client.name ~= "cssls" then
      return
    end
    if not vim.tbl_contains({ "css", "scss", "less" }, vim.bo[e.buf].filetype) then
      return
    end

    vim.keymap.set("n", "<leader>cv", function()
      local word = vim.fn.expand("<cword>")
      if not word:match("^%-%-") then
        vim.notify("[css] cursor is not on a CSS custom property (--var)", vim.log.levels.INFO)
        return
      end
      local ok, tb = pcall(require, "telescope.builtin")
      if not ok then
        vim.lsp.buf.definition()
        return
      end
      pcall(tb.live_grep, {
        default_text = word .. ":",
        cwd = shared.project_root(),
        prompt_title = "CSS variable definition: " .. word,
      })
    end, { buffer = e.buf, desc = "CSS jump to variable definition" })
  end,
  desc = "Register the CSS variable jump keymap when cssls attaches",
})

return {
  shared.treesitter({ "css", "scss" }),
}

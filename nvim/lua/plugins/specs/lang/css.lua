-- lua/plugins/specs/lang/css.lua — CSS, CSS modules and Tailwind
--

local shared = require("plugins.specs.lang.shared")

-- stylelint lints only projects that configure it.
shared.gate_linter({ "css", "scss", "less" }, "stylelint", {
  files = {
    ".stylelintrc", ".stylelintrc.json", ".stylelintrc.js", ".stylelintrc.cjs",
    ".stylelintrc.yaml", ".stylelintrc.yml", "stylelint.config.js", "stylelint.config.cjs",
  },
  pkg_key = "stylelint",
  bin     = "stylelint",
})

-- ── CSS modules LSP ───────────────────────────────────────────────────────────
local _cssmodules_enabled = false
local _cssmodules_missing_reported = false

local function enable_cssmodules_lsp()
  if _cssmodules_enabled then return end
  if vim.fn.executable("cssmodules-language-server") ~= 1 then
    if not _cssmodules_missing_reported then
      _cssmodules_missing_reported = true
      vim.notify(
        "[css] cssmodules-language-server not found — CSS module completion unavailable.\n"
        .. "Install: npm i -g cssmodules-language-server",
        vim.log.levels.DEBUG
      )
    end
    return
  end
  vim.lsp.config("cssmodules_ls", {
    init_options = { isCSSModules = true },
    filetypes    = { "css", "scss", "less", "typescriptreact", "javascriptreact" },
  })
  vim.lsp.enable("cssmodules_ls")
  _cssmodules_enabled = true
end

vim.api.nvim_create_autocmd("BufReadPost", {
  pattern  = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
  once     = true,
  group    = vim.api.nvim_create_augroup("CssModulesLspInit", { clear = true }),
  callback = function() vim.schedule(enable_cssmodules_lsp) end,
  desc     = "Enable the cssmodules LSP on the first CSS/TSX/JSX buffer",
})

-- ── CSS custom-property jump ──────────────────────────────────────────────────
vim.api.nvim_create_autocmd("LspAttach", {
  group    = vim.api.nvim_create_augroup("CssVarJump", { clear = true }),
  callback = function(e)
    local client = vim.lsp.get_client_by_id(e.data.client_id)
    if not client or client.name ~= "cssls" then return end
    if not vim.tbl_contains({ "css", "scss", "less" }, vim.bo[e.buf].filetype) then return end

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
        cwd          = shared.project_root(),
        prompt_title = "CSS variable definition: " .. word,
      })
    end, { buffer = e.buf, desc = "CSS jump to variable definition" })
  end,
  desc = "Register the CSS variable jump keymap when cssls attaches",
})

-- ── Tailwind ──────────────────────────────────────────────────────────────────
local TAILWIND_CONFIGS = {
  "tailwind.config.js", "tailwind.config.ts", "tailwind.config.cjs",
  "tailwind.config.mjs", "tailwind.config.cts", "tailwind.config.mts",
}

return {
  {
    "luckasRanarison/tailwind-tools.nvim",
    cond = function()
      for _, name in ipairs(TAILWIND_CONFIGS) do
        if vim.fn.findfile(name, ".;") ~= "" then return true end
      end
      return false
    end,
    ft           = shared.WEB_FT,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      document_color = { enabled = true, kind = "inline" },
      conceal        = { enabled = false },
      server         = { override = false },
    },
    config = function(_, opts)
      local ok, tt = pcall(require, "tailwind-tools")
      if not ok then
        vim.notify("[css] tailwind-tools.nvim did not load — run :Lazy install", vim.log.levels.WARN)
        return
      end
      local ok_setup, err = pcall(tt.setup, opts)
      if not ok_setup then
        vim.notify("[css] tailwind-tools.nvim setup failed: " .. tostring(err), vim.log.levels.WARN)
      end
    end,
  },

  shared.treesitter({ "css", "scss" }),
}

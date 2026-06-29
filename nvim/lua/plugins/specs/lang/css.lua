-- lua/plugins/specs/lang/css.lua — CSS development
--

local shared = require("plugins.specs.lang.shared")

-- ── Stylelint config detection ────────────────────────────────────────────────
local function has_stylelint_config()
  local ok_path, path = pcall(require, "core.util.path")
  local root = (ok_path and path.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return false end
  local candidates = { ".stylelintrc", ".stylelintrc.json", ".stylelintrc.js",
                       ".stylelintrc.cjs", ".stylelintrc.yaml", ".stylelintrc.yml",
                       "stylelint.config.js", "stylelint.config.cjs" }
  for _, f in ipairs(candidates) do
    if vim.fn.filereadable(root .. "/" .. f) == 1 then return true end
  end
  -- Also check package.json for a "stylelint" key.
  local pkg = root .. "/package.json"
  if vim.fn.filereadable(pkg) == 1 then
    local ok, lines = pcall(vim.fn.readfile, pkg)
    if ok then
      local content = table.concat(lines, "\n")
      local ok_j, obj = pcall(vim.json.decode, content)
      if ok_j and type(obj) == "table" and obj["stylelint"] then return true end
    end
  end
  return false
end

vim.api.nvim_create_autocmd("FileType", {
  pattern  = { "css", "scss", "less" },
  once     = true,
  group    = vim.api.nvim_create_augroup("StylelintConditional", { clear = true }),
  callback = function()
    if vim.fn.executable("stylelint") ~= 1 then return end
    if not has_stylelint_config() then
      vim.notify(
        "[css] stylelint found but no config detected — linter skipped.\n"
        .. "Create .stylelintrc.json to enable it.",
        vim.log.levels.DEBUG
      )
      return
    end
    local ok, lint = pcall(require, "lint")
    if not ok then return end
    for _, ft in ipairs({ "css", "scss", "less" }) do
      lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
      if not vim.tbl_contains(lint.linters_by_ft[ft], "stylelint") then
        table.insert(lint.linters_by_ft[ft], "stylelint")
      end
    end
    vim.notify("[css] stylelint registered (config detected)", vim.log.levels.DEBUG)
  end,
  desc = "Conditionally register stylelint when config is present",
})

-- ── cssmodules LSP setup helper ───────────────────────────────────────────────
--
local _cssmodules_setup_done = false

local function setup_cssmodules_lsp()
  if vim.fn.executable("cssmodules-language-server") ~= 1 then
    if not _cssmodules_setup_done then
      vim.schedule(function()
        vim.notify(
          "[css] cssmodules-language-server not found — CSS module completion unavailable.\n"
          .. "Install: npm i -g cssmodules-language-server",
          vim.log.levels.DEBUG
        )
      end)
      _cssmodules_setup_done = true
    end
    return
  end

  local cfg = {
    init_options = { isCSSModules = true },
    filetypes    = { "css","scss","less","typescriptreact","javascriptreact" },
  }

  if vim.fn.has("nvim-0.11") == 1 then
    pcall(function()
      vim.lsp.config("cssmodules_ls", cfg)
      vim.lsp.enable("cssmodules_ls")
    end)
  else
    local ok, lspconfig = pcall(require, "lspconfig")
    if ok then pcall(function() lspconfig.cssmodules_ls.setup(cfg) end) end
  end

  _cssmodules_setup_done = true
end

return {
  -- ── cssmodules LSP ──────────────────────────────────────────────────────────
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
        once     = true,
        group    = vim.api.nvim_create_augroup("CssModulesLspInit", { clear = true }),
        callback = function() vim.schedule(setup_cssmodules_lsp) end,
        desc     = "Setup cssmodules LSP on first CSS/TSX/JSX buffer",
      })

      vim.api.nvim_create_autocmd("DirChanged", {
        group    = vim.api.nvim_create_augroup("CssModulesLspDir", { clear = true }),
        callback = function()
          _cssmodules_setup_done = false
          -- Only act when a relevant filetype is currently open.
          for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            if vim.api.nvim_buf_is_loaded(buf) then
              local ft = vim.bo[buf].filetype
              if vim.tbl_contains({ "css","scss","less","typescriptreact","javascriptreact" }, ft) then
                vim.schedule(setup_cssmodules_lsp)
                return
              end
            end
          end
        end,
        desc = "Re-setup cssmodules LSP when working directory changes",
      })
    end,
  },

  -- ── CSS variable jump-to-definition ──────────────────────────────────────────
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("LspAttach", {
        group    = vim.api.nvim_create_augroup("CssVarJump", { clear = true }),
        callback = function(e)
          local client = vim.lsp.get_client_by_id(e.data.client_id)
          if not client or client.name ~= "cssls" then return end
          local ft = vim.bo[e.buf].filetype
          if not vim.tbl_contains({ "css","scss","less" }, ft) then return end

          vim.keymap.set("n", "<leader>cv", function()
            local word = vim.fn.expand("<cword>")
            if not word:match("^%-%-") then
              vim.notify("[css] cursor is not on a CSS custom property (--var)", vim.log.levels.INFO)
              return
            end
            local ok, tb = pcall(require, "telescope.builtin")
            if ok then
              local ok_path, path = pcall(require, "core.util.path")
              local root = (ok_path and path.find_root()) or vim.fn.getcwd()
              pcall(tb.live_grep, {
                default_text = word .. ":",
                cwd          = root,
                prompt_title = "CSS variable definition: " .. word,
                type_filter  = "css,scss,less",
              })
            else
              vim.lsp.buf.definition()
            end
          end, { buffer = e.buf, desc = "CSS jump to variable definition" })
        end,
        desc = "Register CSS variable jump keymap on cssls attach",
      })
    end,
  },

  -- ── Tailwind CSS ───────────────────────────────────────────────────────────
  ---- MAINTAINER NOTE: Verify the repository exists before upgrading this config:
  --   https://github.com/laytan/tailwind-tools.nvim
  --
  {
    "laytan/tailwind-tools.nvim",
    cond = function()
      return vim.fn.findfile("tailwind.config.js",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.ts",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.cjs", ".;") ~= ""
    end,
    ft           = shared.WEB_FT,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      document_color = { enabled = true, kind = "inline" },
      conceal        = { enabled = false },
      server         = { override = false },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("tailwind-tools").setup(opts) end)
      if not ok then
        vim.notify(
          "[css] tailwind-tools.nvim failed to load: " .. tostring(err) .. "\n"
          .. "Possible causes:\n"
          .. "  1. The plugin repository name is incorrect.\n"
          .. "     Expected: https://github.com/laytan/tailwind-tools.nvim\n"
          .. "  2. The plugin has not been installed. Run: :Lazy install\n"
          .. "  3. The plugin has been renamed. Update the spec name in css.lua.",
          vim.log.levels.WARN
        )
      end
    end,
  },

  shared.treesitter({ "css", "scss" }),
}

-- lua/plugins/specs/lang/css.lua — CSS development
--

local shared = require("plugins.specs.lang.shared")

-- ── Stylelint config detection ────────────────────────────────────────────
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

-- Register stylelint conditionally — only when a config file is present.
vim.api.nvim_create_autocmd("FileType", {
  pattern  = { "css", "scss", "less" },
  once     = true,
  group    = vim.api.nvim_create_augroup("StylelintConditional", { clear = true }),
  callback = function()
    if not vim.fn.executable("stylelint") == 1 then return end
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

return {
  -- ── cssmodules LSP ──────────────────────────────────────────────────────────
  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern  = { "*.css", "*.scss", "*.less", "*.tsx", "*.jsx" },
        once     = true,
        group    = vim.api.nvim_create_augroup("CssModulesLsp", { clear = true }),
        callback = function()
          if vim.fn.executable("cssmodules-language-server") ~= 1 then
            vim.schedule(function()
              vim.notify(
                "[css] cssmodules-language-server not found — CSS module completion unavailable.\n"
                .. "Install: npm i -g cssmodules-language-server",
                vim.log.levels.DEBUG
              )
            end)
            return
          end
          local cfg = {
            init_options = { isCSSModules = true },
            filetypes    = { "css","scss","less","typescriptreact","javascriptreact" },
          }
          if vim.fn.has("nvim-0.11") == 1 then
            pcall(function() vim.lsp.config("cssmodules_ls", cfg); vim.lsp.enable("cssmodules_ls") end)
          else
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then pcall(function() lspconfig.cssmodules_ls.setup(cfg) end) end
          end
        end,
      })
    end,
  },

  -- ── CSS variable jump-to-definition ──────────────────────────────────────
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
            -- Get the word under cursor; if it starts with -- treat as CSS variable.
            local word = vim.fn.expand("<cword>")
            if not word:match("^%-%-") then
              vim.notify("[css] cursor is not on a CSS custom property (--var)", vim.log.levels.INFO)
              return
            end
            -- Use Telescope live_grep to find the definition.
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
  {
    "luckasRanaringer/tailwind-tools.nvim",
    cond = function()
      return vim.fn.findfile("tailwind.config.js",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.ts",  ".;") ~= ""
          or vim.fn.findfile("tailwind.config.cjs", ".;") ~= ""
    end,
    ft           = shared.WEB_FT,
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = { document_color = { enabled = true, kind = "inline" }, conceal = { enabled = false }, server = { override = false } },
    config = function(_, opts)
      local ok, err = pcall(function() require("tailwind-tools").setup(opts) end)
      if not ok then vim.notify("tailwind-tools setup failed: " .. tostring(err), vim.log.levels.WARN) end
    end,
  },

  shared.treesitter({ "css", "scss" }),
}

-- lua/plugins/specs/lang/html.lua — HTML development
--

local shared = require("plugins.specs.lang.shared")

-- ── htmlhint config detection ──────────────────────────────────────────────
--

local function has_htmlhint_config()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return false end

  local candidates = {
    root .. "/.htmlhintrc",
    root .. "/.htmlhint.json",
    root .. "/.htmlhint.js",
    root .. "/htmlhint.config.js",
  }
  for _, f in ipairs(candidates) do
    if vim.fn.filereadable(f) == 1 then return true end
  end
  return false
end

vim.api.nvim_create_autocmd("FileType", {
  pattern  = { "html", "htmldjango" },
  once     = true,
  group    = vim.api.nvim_create_augroup("HtmlHintConditional", { clear = true }),
  callback = function()
    if not vim.fn.executable("htmlhint") == 1 then return end
    if not has_htmlhint_config() then
      vim.notify(
        "[html] htmlhint found but no .htmlhintrc detected — linter skipped.\n"
        .. "Create a .htmlhintrc to enable: https://htmlhint.com/docs/user-guide/configuration",
        vim.log.levels.DEBUG
      )
      return
    end
    -- Patch nvim-lint if it is loaded.
    local ok, lint = pcall(require, "lint")
    if not ok then return end
    lint.linters_by_ft = lint.linters_by_ft or {}
    lint.linters_by_ft.html = lint.linters_by_ft.html or {}
    local already = false
    for _, l in ipairs(lint.linters_by_ft.html) do
      if l == "htmlhint" then already = true; break end
    end
    if not already then
      table.insert(lint.linters_by_ft.html, "htmlhint")
    end
  end,
  desc = "Conditionally register htmlhint when .htmlhintrc is present",
})

return {
  shared.treesitter({ "html" }),

  -- ── LSP: html-lsp — SOLE owner of html server config ──────────────────────

  {
    "neovim/nvim-lspconfig",
    optional = true,
    init = function()
      local cfg = {
        filetypes    = { "html", "htmldjango", "jinja.html" },
        init_options = {
          provideFormatter = false,
        },
      }

      if vim.fn.executable("vscode-html-language-server") ~= 1 then
        vim.schedule(function()
          vim.notify(
            "[html] vscode-html-language-server not found — run :MasonInstall html-lsp",
            vim.log.levels.DEBUG
          )
        end)
        return
      end

      if vim.fn.has("nvim-0.11") == 1 then
        pcall(function()
          vim.lsp.config("html", cfg)
          vim.lsp.enable("html")
        end)
      else
        vim.api.nvim_create_autocmd("BufReadPost", {
          pattern  = { "*.html", "*.htmldjango", "*.jinja" },
          once     = true,
          group    = vim.api.nvim_create_augroup("HtmlLspCfg", { clear = true }),
          callback = function()
            local ok, lspconfig = pcall(require, "lspconfig")
            if ok then pcall(function() lspconfig.html.setup(cfg) end) end
          end,
          desc = "Register html-lsp on first HTML buffer (legacy path)",
        })
      end
    end,
  },

  -- ── LuaSnip snippets ────────────────────────────────────────────────────────

  {
    "L3MON4D3/LuaSnip",
    optional = true,
    ft       = { "html", "htmldjango" },
    config   = function()
      require("core.util.snippets").load("html", function(s, t, i, _, ref)
        return {
          s("html5", {
            t({
              "<!DOCTYPE html>",
              '<html lang="',
            }),
            i(1, "en"),
            t({ '">',
              "<head>",
              '  <meta charset="UTF-8" />',
              '  <meta name="viewport" content="width=device-width, initial-scale=1.0" />',
              "  <title>",
            }),
            i(2, "Document"),
            t({ "</title>",
              "</head>",
              "<body>",
              "  ",
            }),
            i(0),
            t({ "",
              "</body>",
              "</html>",
            }),
          }),
          s("tag", {
            t("<"), i(1, "div"),
            t(' class="'), i(2), t('">'),
            t({ "", "  " }), i(0),
            t({ "", "</" }), ref(1, "div"), t(">"),
          }),
          s("inp", {
            t('<input type="'), i(1, "text"),
            t('" name="'), i(2),
            t('" id="'), ref(2),
            t('" placeholder="'), i(3), t('" />'),
          }),
        }
      end)
    end,
  },
}

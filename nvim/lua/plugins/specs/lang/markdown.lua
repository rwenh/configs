-- lua/plugins/specs/lang/markdown.lua — Markdown development
--

local icons = require("core.util.icons")

-- ── Word count ────────────────────────────────────────────────────────

local M = {}

function M.word_count(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ft = vim.bo[buf].filetype
  if ft ~= "markdown" and ft ~= "markdown_inline" then return nil end

  local ok, wc = pcall(vim.fn.wordcount)
  if ok and type(wc) == "table" and type(wc.words) == "number" then
    return wc.words
  end

  -- Fallback: count whitespace-separated tokens, skipping frontmatter.
  local lines  = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local count  = 0
  local in_fm  = false

  for idx, line in ipairs(lines) do
    if idx == 1 and line == "---" then
      in_fm = true
    elseif in_fm and line == "---" then
      in_fm = false
    elseif not in_fm then
      local plain = line
        :gsub("%[(.-)%]%(.-%)", "%1")
        :gsub("[#*_`~>]", " ")
        :gsub("%-%-%-+", "")
      for _ in plain:gmatch("%S+") do count = count + 1 end
    end
  end
  return count
end

M.lualine_wordcount = {
  function()
    local n = M.word_count()
    if not n then return "" end
    return icons.status.word_count .. n .. "w"
  end,
  cond = function()
    local ft = vim.bo.filetype
    return ft == "markdown" or ft == "markdown_inline"
  end,
}

-- ── Frontmatter schema hint ───────────────────────────────────────────

vim.api.nvim_create_autocmd("LspAttach", {
  group    = vim.api.nvim_create_augroup("MarkdownFrontmatter", { clear = true }),
  callback = function(e)
    local client = vim.lsp.get_client_by_id(e.data.client_id)
    if not client or client.name ~= "yamlls" then return end
    local ft = vim.bo[e.buf].filetype
    if ft ~= "markdown" and ft ~= "yaml" then return end

    local schemas = client.config.settings
      and client.config.settings.yaml
      and client.config.settings.yaml.schemas
      or {}
    local name = vim.api.nvim_buf_get_name(e.buf)
    if schemas[name] then return end

    local ok, err = pcall(function()
      client.notify("workspace/didChangeConfiguration", {
        settings = vim.tbl_deep_extend("keep", client.config.settings or {}, {
          yaml = {
            validate   = true,
            hover      = true,
            completion = true,
            format     = { enable = false },
          },
        }),
      })
    end)
    if not ok then
      vim.notify(
        "[markdown] yamlls didChangeConfiguration failed: " .. tostring(err),
        vim.log.levels.DEBUG
      )
    end
  end,
  desc = "Enhance yamlls for Markdown frontmatter buffers",
})

return {
  -- ── Markdown Preview (browser) ─────────────────────────────────────────────
  {
    "iamcco/markdown-preview.nvim",
    cmd   = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft    = { "markdown" },

    build = function(plugin)
      local app_dir = plugin.dir .. "/app"

      if vim.fn.executable("yarn") == 1 then
        -- yarn respects the existing yarn.lock — no lockfile conflict.
        return "cd " .. vim.fn.shellescape(app_dir) .. " && yarn install --frozen-lockfile"
      elseif vim.fn.executable("npm") == 1 then
        vim.notify(
          "[markdown-preview] yarn not found — falling back to npm.\n"
          .. "Consider: npm install -g yarn",
          vim.log.levels.WARN
        )
        return "cd " .. vim.fn.shellescape(app_dir) .. " && npm install --legacy-peer-deps"
      else
        vim.notify(
          "[markdown-preview] Neither yarn nor npm found — build skipped.\n"
          .. "Install yarn (recommended): npm install -g yarn",
          vim.log.levels.ERROR
        )
      end
    end,

    init = function()
      vim.g.mkdp_filetypes  = { "markdown" }
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 1
      vim.g.mkdp_preview_options = {
        sync_scroll_type = "middle",
      }

      -- Warn early if neither yarn nor npm is present.
      if vim.fn.executable("yarn") ~= 1 and vim.fn.executable("npm") ~= 1 then
        vim.notify(
          "[markdown-preview] yarn and npm both missing — browser preview unavailable.\n"
          .. "Install yarn: npm install -g yarn",
          vim.log.levels.WARN
        )
      end
    end,

    keys = {
      { "<leader>mp", "<cmd>MarkdownPreviewToggle<CR>", desc = "Markdown Preview Toggle" },
    },
  },

  -- ── vim-markdown ───────────────────────────────────────────────────────────
  {
    "preservim/vim-markdown",
    ft   = { "markdown" },
    init = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_frontmatter      = 1
      vim.g.vim_markdown_toml_frontmatter = 1
    end,
  },

  -- ── Table mode ─────────────────────────────────────────────────────────────
  {
    "dhruvasagar/vim-table-mode",
    ft   = { "markdown" },
    cmd  = "TableModeToggle",
    keys = {
      { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Table Mode Toggle" },
    },
  },

  -- ── render-markdown.nvim ───────────────────────────────────────────────────
  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft           = { "markdown" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      enabled      = true,
      render_modes = { "n" },
      anti_conceal = {
        enabled = true,
        ignore  = { code_background = true, sign = true },
      },
      heading = {
        enabled = true,
        signs   = icons.headings,
        width   = "block",
      },
      code = {
        enabled   = true,
        sign      = true,
        style     = "full",
        border    = "thin",
        width     = "block",
        left_pad  = 1,
        right_pad = 1,
        min_width = 20,
        above     = "▄",
        below     = "▀",
        highlight = "RenderMarkdownCode",
      },
      bullet = {
        enabled = true,
        icons   = { "●", "○", "◆", "◇" },
      },
      checkbox = {
        enabled   = true,
        unchecked = { icon = "󰄱 " },
        checked   = { icon = "󰱒 " },
      },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("render-markdown").setup(opts) end)
      if not ok then
        vim.notify(
          "render-markdown.nvim setup failed: " .. tostring(err)
          .. "\nRun :Lazy update render-markdown.nvim",
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── Link checker + word count keymaps ─────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    ft   = "markdown",
    keys = {
      {
        "<leader>ml",
        function()
          if vim.fn.executable("markdown-link-check") ~= 1 then
            vim.notify(
              "[markdown] markdown-link-check not found.\n"
              .. "Install: npm i -g markdown-link-check",
              vim.log.levels.WARN
            )
            return
          end
          require("core.util.term").float(
            "markdown-link-check " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "Markdown Check Links",
        ft   = "markdown",
      },
      {
        "<leader>mw",
        function()
          local n = M.word_count()
          if n then
            vim.notify(string.format("[markdown] Word count: %d", n), vim.log.levels.INFO)
          end
        end,
        desc = "Markdown Word Count",
        ft   = "markdown",
      },
    },
  },
}

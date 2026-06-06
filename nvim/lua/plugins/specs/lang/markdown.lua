-- lua/plugins/specs/lang/markdown.lua — Markdown development
--

local icons = require("core.util.icons")

-- ── Word count ────────────────────────────────────────────────────────────────

local M = {}

function M.word_count(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ft = vim.bo[buf].filetype
  if ft ~= "markdown" and ft ~= "markdown_inline" then return nil end

  local ok, wc = pcall(vim.fn.wordcount)
  if ok and type(wc) == "table" and type(wc.words) == "number" then
    return wc.words
  end

  -- Fallback: manual count skipping YAML frontmatter.
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local count = 0
  local in_fm = false

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

-- ── Frontmatter schema hint (yamlls) ─────────────────────────────────────────

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

  -- ── markview.nvim — inline buffer renderer ────────────────────────────────
  --
  -- event = "VeryLazy" ensures treesitter is fully initialised before markview
  -- loads, preventing the "nvim-treesitter not ready" startup warning.
  {
    "OXY2DEV/markview.nvim",
    ft           = { "markdown", "markdown_inline", "quarto", "rmd" },
    event        = "VeryLazy",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    opts = {
      modes        = { "n", "no" },
      hybrid_modes = { "n" },

      headings = {
        enable      = true,
        shift_width = 0,
        heading_1   = { style = "label", sign = icons.headings[1] },
        heading_2   = { style = "label", sign = icons.headings[2] },
        heading_3   = { style = "label", sign = icons.headings[3] },
        heading_4   = { style = "label", sign = icons.headings[4] },
        heading_5   = { style = "label", sign = icons.headings[5] },
        heading_6   = { style = "label", sign = icons.headings[6] },
      },

      code_blocks = {
        enable     = true,
        style      = "block",
        sign       = true,
        min_width  = 60,
        pad_amount = 2,
        above      = "▄",
        below      = "▀",
      },

      inline_codes = { enable = true },

      checkboxes = {
        enable    = true,
        checked   = { text = "󰱒", hl = "MarkviewCheckboxChecked"   },
        unchecked = { text = "󰄱", hl = "MarkviewCheckboxUnchecked" },
        pending   = { text = "󰥔", hl = "MarkviewCheckboxPending"   },
      },

      bullets = {
        enable  = true,
        markers = { "●", "○", "◆", "◇" },
      },

      tables           = { enable = true, style = "rounded" },
      horizontal_rules = { enable = true },

      links = {
        enable     = true,
        hyperlinks = { enable = true },
        images     = { enable = true },
        emails     = { enable = true },
      },
    },

    config = function(_, opts)
      local ok, err = pcall(function() require("markview").setup(opts) end)
      if not ok then
        vim.notify(
          "[markdown] markview.nvim setup failed: " .. tostring(err)
          .. "\nRun :Lazy update markview.nvim",
          vim.log.levels.WARN
        )
      end
    end,

    keys = {
      {
        "<leader>mv",
        function() pcall(function() require("markview").toggle() end) end,
        desc = "Markdown Toggle Render (markview)",
        ft   = "markdown",
      },
    },
  },

  -- ── peek.nvim — browser preview ───────────────────────────────────────────
  --
  {
    "toppair/peek.nvim",
    ft    = { "markdown" },
    build = "deno task --quiet build:fast",

    config = function()
      if vim.fn.executable("deno") ~= 1 then
        vim.notify(
          "[markdown] deno not found — browser preview unavailable.\n"
          .. "Install: sudo zypper in deno  (or: cargo install deno)",
          vim.log.levels.WARN
        )
        return
      end

      local ok, peek = pcall(require, "peek")
      if not ok then return end
      pcall(peek.setup, {
        auto_load        = false,
        close_on_bdelete = true,
        syntax           = true,
        theme            = "dark",
        update_on_change = true,
        app              = "browser",
        filetype         = { "markdown" },
        throttle_at      = 200000,
        throttle_time    = "auto",
      })
    end,

    keys = {
      {
        "<leader>mp",
        function()
          local ok, peek = pcall(require, "peek")
          if not ok then return end
          if peek.is_open() then
            pcall(peek.close)
          else
            pcall(peek.open)
          end
        end,
        desc = "Markdown Toggle Browser Preview (peek)",
        ft   = "markdown",
      },
    },
  },

  -- ── vim-markdown ──────────────────────────────────────────────────────────
  {
    "preservim/vim-markdown",
    ft   = { "markdown" },
    init = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_frontmatter      = 1
      vim.g.vim_markdown_toml_frontmatter = 1
    end,
  },

  -- ── Table mode ────────────────────────────────────────────────────────────
  {
    "dhruvasagar/vim-table-mode",
    ft   = { "markdown" },
    cmd  = "TableModeToggle",
    keys = {
      { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Table Mode Toggle" },
    },
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
            vim.notify(
              string.format("[markdown] Word count: %d", n),
              vim.log.levels.INFO
            )
          end
        end,
        desc = "Markdown Word Count",
        ft   = "markdown",
      },
    },
  },
}

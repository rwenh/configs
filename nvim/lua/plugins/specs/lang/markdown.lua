-- lua/plugins/specs/lang/markdown.lua — Markdown development
--

local icons = require("core.util.icons")

-- ── Word count ────────────────────────────────────────────────────────────
--
-- Provides M.word_count(buf) → integer.
--
-- Uses Neovim's built-in wordcount() when available (fast, C-level);

local M = {}

function M.word_count(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ft = vim.bo[buf].filetype
  if ft ~= "markdown" and ft ~= "markdown_inline" then return nil end

  -- vim.fn.wordcount() is synchronous and covers the whole buffer.
  local ok, wc = pcall(vim.fn.wordcount)
  if ok and type(wc) == "table" and type(wc.words) == "number" then
    return wc.words
  end

  -- Fallback: count whitespace-separated tokens, skipping frontmatter.
  local lines  = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local count  = 0
  local in_fm  = false
  local fm_end = false

  for idx, line in ipairs(lines) do
    if idx == 1 and line == "---" then
      in_fm = true
    elseif in_fm and line == "---" then
      in_fm  = false
      fm_end = true
    elseif not in_fm then
      fm_end = true
      -- Strip Markdown syntax before counting.
      local plain = line
        :gsub("%[(.-)%]%(.-%)", "%1")   -- links
        :gsub("[#*_`~>]", " ")          -- headings, emphasis, code, blockquote
        :gsub("%-%-%-+", "")            -- hr
      for _ in plain:gmatch("%S+") do count = count + 1 end
    end
    _ = fm_end  -- suppress unused warning
  end
  return count
end

--   lualine_z = { require("plugins.specs.lang.markdown").lualine_wordcount }
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

-- ── Frontmatter schema hint ───────────────────────────────────────────────
--
-- When yamlls is active, associate common frontmatter schemas so keys like
-- `title`, `date`, `tags`, `draft` get completion and validation.

vim.api.nvim_create_autocmd("LspAttach", {
  group    = vim.api.nvim_create_augroup("MarkdownFrontmatter", { clear = true }),
  callback = function(e)
    local client = vim.lsp.get_client_by_id(e.data.client_id)
    if not client or client.name ~= "yamlls" then return end
    local ft = vim.bo[e.buf].filetype
    if ft ~= "markdown" and ft ~= "yaml" then return end

    -- Add Jekyll/Hugo/Zola frontmatter schema if not already present.
    local ok = pcall(function()
      local current = client.config.settings
        and client.config.settings.yaml
        and client.config.settings.yaml.schemas
        or {}
      -- Only inject when no schema is already set for the buffer's file.
      local name = vim.api.nvim_buf_get_name(e.buf)
      if current[name] then return end

      -- Request the server to use the generic YAML schema.
      -- Projects with stricter schemas should configure yamlls themselves.
      client.notify("workspace/didChangeConfiguration", {
        settings = vim.tbl_deep_extend("keep", client.config.settings or {}, {
          yaml = {
            validate         = true,
            hover            = true,
            completion       = true,
            format           = { enable = false },
          },
        }),
      })
    end)
    _ = ok  -- non-fatal
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
      if vim.fn.executable("npm") ~= 1 then
        vim.notify(
          "[markdown-preview] npm not found — browser preview will not work.\n"
          .. "Install npm (nodejs) and run :Lazy build markdown-preview.nvim.",
          vim.log.levels.WARN
        )
        return
      end

      local out = vim.fn.system(
        "cd " .. vim.fn.shellescape(plugin.dir) .. "/app"
        .. " && npm install --legacy-peer-deps"
      )

      if vim.v.shell_error ~= 0 then
        vim.notify(
          "[markdown-preview] npm install failed — browser preview will not work.\n"
          .. "Run :Lazy build markdown-preview.nvim to retry.\n"
          .. "npm output:\n" .. vim.trim(out),
          vim.log.levels.WARN
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

  -- ── Link checker keymap ────────────────────────────────────────────────────
  --
  -- Runs markdown-link-check (npm i -g markdown-link-check) on the current file.

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

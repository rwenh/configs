-- lua/plugins/specs/lang/markdown.lua — Markdown development
--

local icons  = require("core.util.icons")
local shared = require("plugins.specs.lang.shared")

local function notify(msg, level)
  vim.notify("[markdown] " .. msg, level or vim.log.levels.WARN)
end

-- ── Word count ────────────────────────────────────────────────────────────────
---@param lines string[]
---@return integer
local function count_prose_words(lines)
  local count = 0
  local in_frontmatter, in_code = false, false
  local code_char, code_len = nil, 0

  for idx, line in ipairs(lines) do
    if idx == 1 and line:match("^%-%-%-%s*$") then
      in_frontmatter = true
    elseif in_frontmatter then
      if line:match("^%-%-%-%s*$") or line:match("^%.%.%.%s*$") then in_frontmatter = false end
    elseif in_code then
      local close = line:match("^%s*(`+)%s*$") or line:match("^%s*(~+)%s*$")
      if close and close:sub(1, 1) == code_char and #close >= code_len then
        in_code = false
      end
    else
      local open = line:match("^%s*(```+)") or line:match("^%s*(~~~+)")
      if open then
        in_code, code_char, code_len = true, open:sub(1, 1), #open
      else
        local text = line
          :gsub("<!%-%-.-%-%->", " ")   -- inline HTML comments
          :gsub("`[^`]*`", " ")         -- inline code
          :gsub("!%[(.-)%]%b()", "%1")  -- images → alt text
          :gsub("%[(.-)%]%b()", "%1")   -- links → link text
          :gsub("<[^>]+>", " ")         -- HTML tags
          :gsub("https?://%S+", " ")    -- bare URLs
        for token in text:gmatch("%S+") do
          if token:find("[%w\128-\255]") then count = count + 1 end
        end
      end
    end
  end
  return count
end

---@param buf integer?
---@return integer|nil
local function word_count(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ft = vim.bo[buf].filetype
  if ft ~= "markdown" and ft ~= "markdown_inline" then return nil end
  if vim.b[buf].large_file then return nil end

  local tick = vim.api.nvim_buf_get_changedtick(buf)
  local cached = vim.b[buf]._md_word_count
  if type(cached) == "table" and cached.tick == tick then return cached.words end

  local words = count_prose_words(vim.api.nvim_buf_get_lines(buf, 0, -1, false))
  vim.b[buf]._md_word_count = { tick = tick, words = words }
  return words
end
local M = { word_count = word_count }

local lualine_wordcount = {
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

-- ── Render toggle (markview.nvim) ─────────────────────────────────────────────
local function toggle_render()
  local ok, actions = pcall(require, "markview.actions")
  if not ok then
    notify("markview.nvim is not loaded — run :Lazy")
    return
  end
  local ok_call, err = pcall(actions.toggle, vim.api.nvim_get_current_buf())
  if not ok_call then notify("render toggle failed: " .. tostring(err), vim.log.levels.ERROR) end
end

-- ── Browser preview (peek.nvim) ───────────────────────────────────────────────
local function toggle_preview()
  if vim.fn.executable("deno") ~= 1 then
    notify("deno not found — browser preview unavailable.\nInstall: sudo zypper in deno  (or: cargo install deno)")
    return
  end
  local ok, peek = pcall(require, "peek")
  if not ok then
    notify("peek.nvim is not loaded — run :Lazy")
    return
  end
  local ok_call, err = pcall(function()
    if peek.is_open() then peek.close() else peek.open() end
  end)
  if not ok_call then notify("preview failed: " .. tostring(err), vim.log.levels.ERROR) end
end

-- ── Frontmatter schema hint (yamlls) ──────────────────────────────────────────
local _yamlls_configured = {}

vim.api.nvim_create_autocmd("LspAttach", {
  group    = vim.api.nvim_create_augroup("MarkdownFrontmatter", { clear = true }),
  callback = function(e)
    local client = vim.lsp.get_client_by_id(e.data.client_id)
    if not client or client.name ~= "yamlls" then return end
    local ft = vim.bo[e.buf].filetype
    if ft ~= "markdown" and ft ~= "yaml" then return end
    if _yamlls_configured[client.id] then return end
    _yamlls_configured[client.id] = true

    local ok, err = pcall(function()
      client.notify("workspace/didChangeConfiguration", {
        settings = vim.tbl_deep_extend("keep", client.config.settings or {}, {
          yaml = { validate = true, hover = true, completion = true, format = { enable = false } },
        }),
      })
    end)
    if not ok then
      _yamlls_configured[client.id] = nil
      notify("yamlls configuration update failed: " .. tostring(err), vim.log.levels.DEBUG)
    end
  end,
  desc = "Enhance yamlls for markdown frontmatter buffers (once per client)",
})

return {

  -- ── Word count in the statusline ───────────────────────────────────────────
  {
    "nvim-lualine/lualine.nvim",
    optional = true,
    opts = function(_, opts)
      opts.sections = opts.sections or {}
      opts.sections.lualine_x = opts.sections.lualine_x or {}
      table.insert(opts.sections.lualine_x, 1, lualine_wordcount)
    end,
  },

  -- ── markview.nvim — inline renderer ────────────────────────────────────────
  {
    "OXY2DEV/markview.nvim",
    ft           = { "markdown", "markdown_inline", "quarto", "rmd" },
    event        = "VeryLazy",
    dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
    -- markview >= 25 nests options per language (preview / markdown / markdown_inline).
    opts = {
      preview = { modes = { "n", "no" }, hybrid_modes = { "n" } },
      markdown = {
        headings = {
          enable = true, shift_width = 0,
          heading_1 = { sign = icons.headings[1] },
          heading_2 = { sign = icons.headings[2] },
          heading_3 = { sign = icons.headings[3] },
          heading_4 = { sign = icons.headings[4] },
          heading_5 = { sign = icons.headings[5] },
          heading_6 = { sign = icons.headings[6] },
        },
        code_blocks      = { enable = true, min_width = 60, pad_amount = 2 },
        tables           = { enable = true },
        horizontal_rules = { enable = true },
      },
      markdown_inline = {
        inline_codes = { enable = true },
        checkboxes   = { enable = true },
        hyperlinks   = { enable = true },
        images       = { enable = true },
        emails       = { enable = true },
      },
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("markview").setup(opts) end)
      if not ok then
        notify("markview.nvim setup failed: " .. tostring(err) .. "\nRun :Lazy update markview.nvim")
      end
    end,
    keys = {
      { "<leader>mv", toggle_render, desc = "Markdown toggle render (markview)", ft = "markdown" },
    },
  },

  -- ── peek.nvim — browser preview ────────────────────────────────────────────
  {
    "toppair/peek.nvim",
    ft    = { "markdown" },
    build = "deno task --quiet build:fast",
    config = function()
      -- toggle_preview() explains a missing deno when the key is pressed.
      if vim.fn.executable("deno") ~= 1 then return end
      local ok, peek = pcall(require, "peek")
      if not ok then return end
      local ok_setup, err = pcall(peek.setup, {
        auto_load        = false,
        close_on_bdelete = true,
        syntax           = true,
        theme            = "dark",
        update_on_change = true,
        app              = "browser",   -- opened with the default browser (see peek's main.ts)
        filetype         = { "markdown" },
        throttle_at      = 200000,
        throttle_time    = "auto",
      })
      if not ok_setup then notify("peek.nvim setup failed: " .. tostring(err)) end
    end,
    keys = {
      { "<leader>mp", toggle_preview, desc = "Markdown toggle browser preview (peek)", ft = "markdown" },
    },
  },

  -- ── vim-markdown (syntax + frontmatter; folding is handled by treesitter) ──
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
      { "<leader>tm", "<cmd>TableModeToggle<CR>", desc = "Table mode toggle" },
    },
  },

  -- ── Link checker and word count keymaps ────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    ft   = "markdown",
    keys = {
      {
        "<leader>ml",
        function()
          if vim.fn.executable("markdown-link-check") ~= 1 then
            notify("markdown-link-check not found.\nInstall: npm i -g markdown-link-check")
            return
          end
          require("core.util.term").float(
            "markdown-link-check " .. vim.fn.shellescape(vim.fn.expand("%:p"))
          )
        end,
        desc = "Markdown check links",
        ft   = "markdown",
      },
      {
        "<leader>mw",
        function()
          local n = M.word_count()
          if n then
            notify(string.format("Word count: %d", n), vim.log.levels.INFO)
          else
            notify("Word count unavailable for this buffer (large file or not markdown)")
          end
        end,
        desc = "Markdown word count",
        ft   = "markdown",
      },
    },
  },
}

-- ~/.config/nvim/lua/config/ui.lua

local helpers = require("utils.helpers")
local theme = require("core.theme")

-- Lualine
helpers.setup_plugin("lualine", function(lualine)
  local c = theme.colors
  
  -- Custom components
  local function lsp_status()
    local clients = vim.lsp.get_clients({ bufnr = 0 })
    if #clients == 0 then return '' end
    local names = {}
    for _, client in pairs(clients) do
      -- Skip null-ls as it's internal
      if client.name ~= "null-ls" then
        table.insert(names, client.name)
      end
    end
    return #names > 0 and (' ' .. table.concat(names, ',')) or ''
  end
  
  local function dap_status()
    local dap_ok, dap = pcall(require, "dap")
    if not dap_ok then return '' end
    local status = dap.status()
    return status ~= '' and ('  ' .. status) or ''
  end
  
  lualine.setup({
    options = {
      theme = {
        normal = {
          a = { fg = c.base03, bg = c.blue, gui = 'bold' },
          b = { fg = c.base0, bg = c.base02 },
          c = { fg = c.base1, bg = c.base03 },
        },
        insert = { a = { fg = c.base03, bg = c.green, gui = 'bold' } },
        visual = { a = { fg = c.base03, bg = c.magenta, gui = 'bold' } },
        replace = { a = { fg = c.base03, bg = c.red, gui = 'bold' } },
        command = { a = { fg = c.base03, bg = c.orange, gui = 'bold' } },
      },
      globalstatus = true,
      section_separators = { left = '', right = '' },
      component_separators = { left = '', right = '' },
      disabled_filetypes = { 
        statusline = { 'dashboard', 'alpha', 'starter' },
        winbar = {},
      },
    },
    sections = {
      lualine_a = { { 'mode', fmt = function(s) return s:sub(1,1) end } },
      lualine_b = { 'branch', 'diff' },
      lualine_c = {
        { 'filename', path = 1, symbols = { modified = '●', readonly = '' } },
        { dap_status, color = { fg = c.orange } },
      },
      lualine_x = {
        {
          'diagnostics',
          sources = { 'nvim_diagnostic' },
          symbols = { error = ' ', warn = ' ', info = ' ', hint = '󰌵 ' },
        },
        { lsp_status, color = { fg = c.green } },
        'encoding',
        'fileformat',
        'filetype',
      },
      lualine_y = { 'progress' },
      lualine_z = { 'location' }
    },
  })
end)

-- Bufferline
helpers.setup_plugin("bufferline", function(bufferline)
  bufferline.setup({
    options = {
      mode = "buffers",
      numbers = "ordinal",
      close_command = "bd %d",
      right_mouse_command = "bd %d",
      indicator = { style = 'underline' },
      buffer_close_icon = '',
      modified_icon = '●',
      close_icon = '',
      left_trunc_marker = '',
      right_trunc_marker = '',
      diagnostics = "nvim_lsp",
      diagnostics_indicator = function(count, level)
        local icon = level:match("error") and " " or " "
        return " " .. icon .. count
      end,
      offsets = {
        {
          filetype = "NvimTree",
          text = "Explorer",
          text_align = "center",
          separator = true
        }
      },
      color_icons = true,
      show_buffer_icons = true,
      show_buffer_close_icons = true,
      show_close_icon = false,
      show_tab_indicators = true,
      separator_style = "thin",
      always_show_bufferline = false,
      hover = {
        enabled = true,
        delay = 200,
        reveal = {'close'}
      },
    },
  })
end)

-- Dashboard
helpers.setup_plugin("dashboard", function(dashboard)
  dashboard.setup({
    theme = 'doom',
    config = {
      header = {
        '',
        '███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗',
        '████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║',
        '██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║',
        '██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║',
        '██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║',
        '╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝',
        '',
      },
      center = {
        { icon = ' ', desc = 'Find File         ', key = 'f', action = 'Telescope find_files' },
        { icon = ' ', desc = 'Recent Files      ', key = 'r', action = 'Telescope oldfiles' },
        { icon = ' ', desc = 'Find Text         ', key = 'g', action = 'Telescope live_grep' },
        { icon = ' ', desc = 'Restore Session   ', key = 's', action = 'lua require("persistence").load()' },
        { icon = ' ', desc = 'Config            ', key = 'c', action = 'edit ~/.config/nvim/init.lua' },
        { icon = ' ', desc = 'Health Check      ', key = 'h', action = 'Health' },
        { icon = ' ', desc = 'Lazy              ', key = 'l', action = 'Lazy' },
        { icon = ' ', desc = 'Quit              ', key = 'q', action = 'quit' },
      },
      footer = function()
        local stats = require("lazy").stats()
        local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
        return { '', '⚡ ' .. stats.loaded .. '/' .. stats.count .. ' plugins loaded in ' .. ms .. 'ms' }
      end,
    },
  })
end)

-- Indent blankline
helpers.setup_plugin("ibl", function(ibl)
  ibl.setup({
    indent = {
      char = "│",
      tab_char = "│",
    },
    scope = {
      enabled = true,
      show_start = false,
      show_end = false,
      highlight = { "Function", "Label" },
    },
    exclude = {
      filetypes = {
        "help", "dashboard", "lazy", "mason",
        "NvimTree", "Trouble", "notify", "toggleterm"
      },
      buftypes = { "terminal", "nofile" },
    },
  })
end)

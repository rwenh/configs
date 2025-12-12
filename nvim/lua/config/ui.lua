-- ~/.config/nvim/lua/config/ui.lua

local helpers = require("utils.helpers")
local theme = require("core.theme")

-- Lualine
helpers.setup_plugin("lualine", function(lualine)
  local c = theme.colors
  
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
      },
      globalstatus = true,
      section_separators = { left = '', right = '' },
      component_separators = { left = '', right = '' },
    },
    sections = {
      lualine_a = { { 'mode', fmt = function(s) return s:sub(1,1) end } },
      lualine_b = { 'branch', 'diff' },
      lualine_c = {
        { 'filename', path = 1, symbols = { modified = '●', readonly = '' } }
      },
      lualine_x = {
        {
          'diagnostics',
          sources = { 'nvim_diagnostic' },
          symbols = { error = ' ', warn = ' ', info = ' ', hint = '󰌵 ' },
        },
        {
          function()
            local clients = vim.lsp.get_clients({ bufnr = 0 })
            if #clients == 0 then return '' end
            local names = {}
            for _, c in pairs(clients) do table.insert(names, c.name) end
            return ' ' .. table.concat(names, ',')
          end,
          color = { fg = c.green },
        },
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
      separator_style = "thin",
      always_show_bufferline = false,
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
        { icon = ' ', desc = 'Find File', key = 'f', action = 'Telescope find_files' },
        { icon = ' ', desc = 'Recent Files', key = 'r', action = 'Telescope oldfiles' },
        { icon = ' ', desc = 'Find Text', key = 'g', action = 'Telescope live_grep' },
        { icon = ' ', desc = 'Config', key = 'c', action = 'edit ~/.config/nvim/init.lua' },
        { icon = ' ', desc = 'Lazy', key = 'l', action = 'Lazy' },
        { icon = ' ', desc = 'Quit', key = 'q', action = 'quit' },
      },
      footer = function()
        local stats = require("lazy").stats()
        return { '', '⚡ ' .. stats.loaded .. '/' .. stats.count .. ' plugins loaded' }
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
    },
    exclude = {
      filetypes = {
        "help", "dashboard", "lazy", "mason",
        "NvimTree", "Trouble", "notify"
      },
    },
  })
end)
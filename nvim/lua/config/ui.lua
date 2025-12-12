-- ~/.config/nvim/lua/config/ui.lua
-- UI plugin configurations

local helpers = require("utils.helpers")
local theme = require("core.theme")

-- Lualine
helpers.setup_plugin("lualine", function(lualine)
  local colors = theme.colors
  
  lualine.setup({
    options = {
      theme = {
        normal = {
          a = { fg = colors.base03, bg = colors.blue, gui = 'bold' },
          b = { fg = colors.base0, bg = colors.base02 },
          c = { fg = colors.base1, bg = colors.base03 },
        },
        insert = { a = { fg = colors.base03, bg = colors.green, gui = 'bold' } },
        visual = { a = { fg = colors.base03, bg = colors.magenta, gui = 'bold' } },
        replace = { a = { fg = colors.base03, bg = colors.red, gui = 'bold' } },
      },
      globalstatus = true,
      section_separators = { left = '', right = '' },
      component_separators = { left = '', right = '' },
    },
    sections = {
      lualine_a = { { 'mode', fmt = function(str) return str:sub(1,1) end } },
      lualine_b = { 'branch', 'diff', 'diagnostics' },
      lualine_c = { { 'filename', path = 1 } },
      lualine_x = {
        {
          function()
            local clients = vim.lsp.get_clients({ bufnr = 0 })
            if next(clients) == nil then return 'No LSP' end
            local names = {}
            for _, client in pairs(clients) do
              table.insert(names, client.name)
            end
            return ' ' .. table.concat(names, ', ')
          end,
        },
        'encoding',
        'fileformat',
        'filetype'
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
      close_command = function(n) helpers.smart_buf_delete() end,
      indicator = { icon = '▎', style = 'icon' },
      buffer_close_icon = '',
      modified_icon = '●',
      close_icon = '',
      diagnostics = "nvim_lsp",
      diagnostics_indicator = function(count, level)
        local icon = level:match("error") and " " or " "
        return " " .. icon .. count
      end,
      offsets = {
        {
          filetype = "NvimTree",
          text = " File Explorer",
          text_align = "left",
          separator = true
        }
      },
      separator_style = "thin",
      always_show_bufferline = true,
    },
  })
end)

-- Dashboard
helpers.setup_plugin("dashboard", function(dashboard)
  dashboard.setup({
    theme = 'hyper',
    config = {
      week_header = { enable = true },
      shortcut = {
        { desc = ' Update', group = '@property', action = 'Lazy update', key = 'u' },
        { desc = ' Files', group = 'Label', action = 'Telescope find_files', key = 'f' },
      },
      packages = { enable = true },
      project = { enable = true, limit = 8 },
      mru = { limit = 10 },
      footer = function()
        local lazy_ok, lazy = pcall(require, "lazy")
        if lazy_ok then
          local stats = lazy.stats()
          return {
            "⚡ " .. stats.loaded .. "/" .. stats.count .. " plugins loaded",
            os.date("%Y-%m-%d %H:%M:%S")
          }
        end
        return {}
      end,
    },
  })
end)

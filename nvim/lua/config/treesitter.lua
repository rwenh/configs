-- ~/.config/nvim/lua/config/treesitter.lua
-- Treesitter configuration

local helpers = require("utils.helpers")

helpers.setup_plugin("nvim-treesitter.configs", function(configs)
  configs.setup({
    ensure_installed = {
      "c", "cpp", "lua", "vim", "vimdoc", "python", "rust", "fortran",
      "sql", "html", "css", "javascript", "typescript", "java", "json",
      "yaml", "bash", "markdown", "markdown_inline"
    },
    
    auto_install = true,
    sync_install = false,
    
    highlight = {
      enable = true,
      disable = function(lang, buf)
        local max_filesize = 100 * 1024
        local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
        return ok and stats and stats.size > max_filesize
      end,
    },
    
    indent = { enable = true, disable = { "python", "yaml" } },
    
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "<C-space>",
        node_incremental = "<C-space>",
        scope_incremental = "<C-s>",
        node_decremental = "<M-space>",
      },
    },
    
    textobjects = {
      select = {
        enable = true,
        lookahead = true,
        keymaps = {
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
        },
      },
      move = {
        enable = true,
        set_jumps = true,
        goto_next_start = { ["]m"] = "@function.outer" },
        goto_previous_start = { ["[m"] = "@function.outer" },
      },
    },
  })
end)

helpers.setup_plugin("treesitter-context", function(context)
  context.setup({ enable = true, max_lines = 4 })
end)

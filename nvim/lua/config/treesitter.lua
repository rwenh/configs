-- ~/.config/nvim/lua/config/treesitter.lua

require("nvim-treesitter.configs").setup({
  ensure_installed = {
    "c", "cpp", "lua", "vim", "vimdoc", "python", "rust",
    "javascript", "typescript", "tsx", "html", "css",
    "json", "yaml", "toml", "bash", "markdown", "markdown_inline",
    "sql", "java", "fortran", "go", "regex"
  },
  
  auto_install = true,
  sync_install = false,
  
  highlight = {
    enable = true,
    disable = function(lang, buf)
      local max_filesize = 100 * 1024
      local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
    additional_vim_regex_highlighting = false,
  },
  
  indent = {
    enable = true,
    disable = { "python", "yaml" },
  },
  
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-space>",
      node_incremental = "<C-space>",
      scope_incremental = false,
      node_decremental = "<bs>",
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
        ["aa"] = "@parameter.outer",
        ["ia"] = "@parameter.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        ["]f"] = "@function.outer",
        ["]c"] = "@class.outer",
      },
      goto_next_end = {
        ["]F"] = "@function.outer",
        ["]C"] = "@class.outer",
      },
      goto_previous_start = {
        ["[f"] = "@function.outer",
        ["[c"] = "@class.outer",
      },
      goto_previous_end = {
        ["[F"] = "@function.outer",
        ["[C"] = "@class.outer",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ["<leader>a"] = "@parameter.inner",
      },
      swap_previous = {
        ["<leader>A"] = "@parameter.inner",
      },
    },
  },
})

require("treesitter-context").setup({
  enable = true,
  max_lines = 3,
  min_window_height = 20,
  trim_scope = 'outer',
  mode = 'cursor',
})
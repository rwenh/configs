-- ~/.config/nvim/lua/config/git.lua
-- Git integration configuration

local helpers = require("utils.helpers")

helpers.setup_plugin("gitsigns", function(gitsigns)
  gitsigns.setup({
    signs = {
      add = { text = "+" },
      change = { text = "~" },
      delete = { text = "_" },
      topdelete = { text = "‾" },
      changedelete = { text = "~" },
    },
    on_attach = function(bufnr)
      local gs = package.loaded.gitsigns
      local function map(mode, l, r, opts)
        opts = opts or {}
        opts.buffer = bufnr
        vim.keymap.set(mode, l, r, opts)
      end
      
      map('n', ']c', function()
        if vim.wo.diff then return ']c' end
        vim.schedule(function() gs.next_hunk() end)
        return '<Ignore>'
      end, {expr=true, desc="Next hunk"})
      
      map('n', '[c', function()
        if vim.wo.diff then return '[c' end
        vim.schedule(function() gs.prev_hunk() end)
        return '<Ignore>'
      end, {expr=true, desc="Previous hunk"})
      
      map('n', '<leader>hs', gs.stage_hunk, {desc="Stage hunk"})
      map('n', '<leader>hr', gs.reset_hunk, {desc="Reset hunk"})
      map('n', '<leader>hp', gs.preview_hunk, {desc="Preview hunk"})
      map('n', '<leader>hb', function() gs.blame_line{full=true} end, {desc="Blame line"})
    end
  })
end)

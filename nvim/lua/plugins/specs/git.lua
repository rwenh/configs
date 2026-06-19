-- lua/plugins/specs/git.lua — git tooling
--

return {

  { "lewis6991/gitsigns.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts  = {
      signs = { add={text="+"}, change={text="~"}, delete={text="_"}, topdelete={text="‾"}, changedelete={text="~"} },
      current_line_blame = false,
      on_attach = function(bufnr)
        local gs  = require("gitsigns")
        local map = vim.keymap.set
        local function visual_lines() return { vim.fn.line("."), vim.fn.line("v") } end
        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk"          })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk"          })
        map("n", "<leader>.p", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
        map("n", "<leader>.r", gs.reset_hunk,   { buffer = bufnr, desc = "Reset hunk"   })
        map("n", "<leader>.S", gs.stage_hunk,   { buffer = bufnr, desc = "Stage hunk"   })
        map("v", "<leader>.r", function() gs.reset_hunk(visual_lines()) end, { buffer = bufnr, desc = "Reset hunk (visual)" })
        map("v", "<leader>.S", function() gs.stage_hunk(visual_lines()) end, { buffer = bufnr, desc = "Stage hunk (visual)" })
      end,
    },
  },

  { "tpope/vim-fugitive", cmd = { "Git","Gdiffsplit","Gread","Gwrite","GMove","GDelete" } },
  { "kdheepak/lazygit.nvim", cmd = "LazyGit", dependencies = "nvim-lua/plenary.nvim" },
  { "sindrets/diffview.nvim", cmd = { "DiffviewOpen","DiffviewFileHistory" }, opts = {} },

  { "NeogitOrg/neogit",
    cmd          = "Neogit",
    dependencies = { "nvim-lua/plenary.nvim","nvim-telescope/telescope.nvim","sindrets/diffview.nvim" },
    keys = {
      { "<leader>.N", "<cmd>Neogit<cr>",        desc = "Neogit UI"  },
      { "<leader>.C", "<cmd>Neogit commit<cr>", desc = "Git commit" },
    },
    opts = {
      disable_line_numbers = false, auto_refresh = true, auto_show_console = false,
      integrations = { diffview = true, telescope = true },
      signs = { hunk = { "", "" }, item = { ">", "v" }, section = { ">", "v" } },
    },
    config = function(_, opts) pcall(function() require("neogit").setup(opts) end) end,
  },

  { "akinsho/git-conflict.nvim",
    event = "VeryLazy",
    keys  = {
      { "<leader>gco", "<cmd>GitConflictChooseOurs<cr>",   desc = "Choose ours"    },
      { "<leader>gct", "<cmd>GitConflictChooseTheirs<cr>", desc = "Choose theirs"  },
      { "<leader>gcb", "<cmd>GitConflictChooseBoth<cr>",   desc = "Choose both"    },
      { "<leader>gc0", "<cmd>GitConflictChooseNone<cr>",   desc = "Choose neither" },
    },
    opts = {
      default_mappings = false, disable_diagnostics = false,
      highlights = { incoming = "DiffAdd", current = "DiffText", ancestor = "DiffChange" },
    },
    config = function(_, opts) pcall(function() require("git-conflict").setup(opts) end) end,
  },

  { "junegunn/gv.vim",
    cmd          = "GV",
    dependencies = "tpope/vim-fugitive",
    keys         = { { "<leader>.v", "<cmd>GV<cr>", desc = "Git history (graph)" } },
  },

  { "pwntester/octo.nvim",
    cmd          = "Octo",
    dependencies = { "nvim-lua/plenary.nvim","nvim-telescope/telescope.nvim","nvim-tree/nvim-web-devicons" },
    keys = {
      { "<leader>.oi", "<cmd>Octo issue list<cr>",   desc = "GitHub issues"   },
      { "<leader>.op", "<cmd>Octo pr list<cr>",      desc = "GitHub PRs"      },
      { "<leader>.or", "<cmd>Octo review start<cr>", desc = "Start PR review" },
      { "<leader>.oc", "<cmd>Octo pr checkout<cr>",  desc = "Checkout PR"     },
    },
    config = function()
      local ok, octo = pcall(require, "octo")
      if not ok then return end

      local opts = {
        use_local_fs   = false,
        default_remote = { "upstream", "origin" },
        picker         = "telescope",
        comment_icon   = "▎",
        file_panel     = { size = 10, use_icons = true },
      }

      local ok_cfg, octo_config = pcall(require, "octo.config")
      if ok_cfg and type(octo_config) == "table" then
        local defaults = type(octo_config.defaults) == "table" and octo_config.defaults
                      or (type(octo_config.get_default_values) == "function" and octo_config.get_default_values())
                      or {}
        if defaults.timeout ~= nil then
          opts.timeout = vim.g.octo_timeout_ms or 10000
        end
      else
        -- Fallback: include timeout anyway; octo will ignore unknown keys.
        opts.timeout = vim.g.octo_timeout_ms or 10000
      end

      pcall(function() octo.setup(opts) end)
    end,
  },

  { "FabijanZulj/blame.nvim",
    cmd  = "BlameToggle",
    keys = { { "<leader>.B", "<cmd>BlameToggle<cr>", desc = "Toggle git blame" } },
    opts = { date_format = "%Y-%m-%d", virtual_style = "right_align", focus_blame = true,
             merge_consecutive = false, max_summary_width = 30 },
  },

  { "tpope/vim-fugitive",
    keys = {
      { "<leader>.zz",
        function()
          vim.ui.input({ prompt = "Stash message (optional): " }, function(msg)
            local cmd = (msg and msg ~= "")
              and ("Git stash push -m " .. vim.fn.shellescape(msg))
              or  "Git stash push"
            pcall(vim.cmd, cmd)
          end)
        end,
        desc = "Git stash push",
      },
      { "<leader>.zp", "<cmd>Git stash pop<cr>",       desc = "Git stash pop"              },
      { "<leader>.zl", "<cmd>Telescope git_stash<cr>", desc = "Git stash list (Telescope)" },
      { "<leader>.zh",
        function()
          local file = vim.fn.expand("%:p")
          if file == "" then vim.notify("[git] no file in current buffer", vim.log.levels.WARN); return end
          pcall(vim.cmd, "DiffviewFileHistory --follow " .. vim.fn.fnameescape(file))
        end,
        desc = "Git file history --follow (rename-tracking)",
      },
    },
  },

  { "ThePrimeagen/git-worktree.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    event        = "VeryLazy",
    config = function()
      local ok, wt = pcall(require, "git-worktree")
      if not ok then return end
      pcall(function() wt.setup() end)
      local ok_t, telescope = pcall(require, "telescope")
      if ok_t then pcall(function() telescope.load_extension("git_worktree") end) end
    end,
    keys = {
      { "<leader>.wl", function()
          local ok, telescope = pcall(require, "telescope")
          if ok then pcall(function() telescope.extensions.git_worktree.git_worktrees() end) end
        end, desc = "Git worktree list / switch" },
      { "<leader>.wc", function()
          local ok, telescope = pcall(require, "telescope")
          if ok then pcall(function() telescope.extensions.git_worktree.create_git_worktree() end) end
        end, desc = "Git worktree create" },
    },
  },
}

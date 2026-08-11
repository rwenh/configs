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

      local user_set_timeout = vim.g.octo_timeout_ms ~= nil

      local ok_cfg, octo_config = pcall(require, "octo.config")
      if ok_cfg and type(octo_config) == "table" then
        local defaults = type(octo_config.defaults) == "table" and octo_config.defaults
                      or (type(octo_config.get_default_values) == "function" and octo_config.get_default_values())
                      or {}
        if defaults.timeout ~= nil then
          opts.timeout = vim.g.octo_timeout_ms or 10000
        elseif user_set_timeout then
          vim.notify(
            string.format(
              "[git] vim.g.octo_timeout_ms = %d is set, but the installed octo.nvim\n"
              .. "version doesn't expose a 'timeout' config field. Skipping it rather\n"
              .. "than passing a value octo.nvim won't recognise.\n"
              .. "Run :Lazy update octo.nvim, or check octo.nvim's current option names.",
              vim.g.octo_timeout_ms
            ),
            vim.log.levels.WARN
          )
        end
      else
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
            if msg == nil then return end

            local cmd = { "git", "stash", "push" }
            if msg ~= "" then
              table.insert(cmd, "-m")
              table.insert(cmd, msg)
            end

            local buf_dir = vim.fn.expand("%:p:h")
            vim.system(cmd, { text = true, cwd = (buf_dir ~= "" and buf_dir or nil) }, function(result)
              vim.schedule(function()
                local out = vim.trim((result.stderr or "") .. (result.stdout or ""))
                if result.code == 0 then
                  vim.notify("[git] " .. (out ~= "" and out or "Stash pushed"), vim.log.levels.INFO)
                else
                  vim.notify("[git] stash push failed: " .. out, vim.log.levels.ERROR)
                end
              end)
            end)
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

  -- ── git-worktree ────────────────────────────────────────────────────────────
  --
  { "ThePrimeagen/git-worktree.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    event        = "VeryLazy",
    config = function()
      local ok, wt = pcall(require, "git-worktree")
      if not ok then return end
      pcall(function() wt.setup() end)
      local ok_t, telescope = pcall(require, "telescope")
      if ok_t then
        local ok_ext, err = pcall(function() telescope.load_extension("git_worktree") end)
        if not ok_ext then
          vim.notify(
            "[git] git_worktree Telescope extension failed to load: " .. tostring(err)
            .. "\n<leader>.wl / <leader>.wc will not work until this is fixed.\n"
            .. "Check :Lazy log git-worktree.nvim for details.",
            vim.log.levels.WARN
          )
        end
      end
    end,
    keys = (function()
      local function call_worktree_ext(fn_name, desc)
        return function()
          local ok, telescope = pcall(require, "telescope")
          if not ok then
            vim.notify("[git] telescope not available", vim.log.levels.WARN)
            return
          end
          local ext = telescope.extensions and telescope.extensions.git_worktree
          if not ext or type(ext[fn_name]) ~= "function" then
            vim.notify(
              "[git] git_worktree Telescope extension is not loaded — " .. desc .. " unavailable.\n"
              .. "It should load automatically; if this persists, check :Lazy log git-worktree.nvim\n"
              .. "or run: :lua require('telescope').load_extension('git_worktree')",
              vim.log.levels.WARN
            )
            return
          end
          local ok_call, err = pcall(ext[fn_name])
          if not ok_call then
            vim.notify("[git] " .. desc .. " failed: " .. tostring(err), vim.log.levels.WARN)
          end
        end
      end
      return {
        { "<leader>.wl", call_worktree_ext("git_worktrees",       "Git worktree list / switch"), desc = "Git worktree list / switch" },
        { "<leader>.wc", call_worktree_ext("create_git_worktree", "Git worktree create"),         desc = "Git worktree create" },
      }
    end)(),
  },
}

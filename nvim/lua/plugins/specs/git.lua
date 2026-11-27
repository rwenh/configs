-- lua/plugins/specs/git.lua — git tooling
--
-- Ownership:
--   gitsigns      hunks, blame, and the stash keys. No plugin provides git stash, and gitsigns is the
--                 git integration that every file buffer loads.
--   diffview      diff views and file history
--   git-conflict  conflict resolution
--   octo          GitHub issues and pull requests
--   git-worktree  worktrees through Telescope
--
-- Each key sits on the spec of the plugin that owns it, so pressing it loads that plugin.

local flags = require("core.flags")

-- ── Stash ─────────────────────────────────────────────────────────────────────

--- Run a git command in the directory of the current buffer, and report the result.
---@param cmd    string[]  full argv, starting with "git"
---@param what   string    short name for the failure message
---@param ok_msg string    shown on success when git prints nothing
local function git_run(cmd, what, ok_msg)
  local dir = vim.fn.expand("%:p:h")
  vim.system(cmd, { text = true, cwd = dir ~= "" and dir or nil }, function(result)
    vim.schedule(function()
      local out = vim.trim((result.stderr or "") .. (result.stdout or ""))
      if result.code == 0 then
        vim.notify("[git] " .. (out ~= "" and out or ok_msg), vim.log.levels.INFO)
      else
        vim.notify("[git] " .. what .. " failed: " .. out, vim.log.levels.ERROR)
      end
    end)
  end)
end

local function stash_push()
  vim.ui.input({ prompt = "Stash message (optional): " }, function(msg)
    if msg == nil then
      return
    end
    local cmd = { "git", "stash", "push" }
    if msg ~= "" then
      vim.list_extend(cmd, { "-m", msg })
    end
    git_run(cmd, "stash push", "Stash pushed")
  end)
end

local function stash_pop()
  git_run({ "git", "stash", "pop" }, "stash pop", "Stash popped")
end

-- ── Worktree ──────────────────────────────────────────────────────────────────

--- Key handler that calls one git-worktree Telescope picker, reporting why it cannot.
---@param fn_name string  function on telescope.extensions.git_worktree
---@param desc    string
---@return fun()
local function worktree_action(fn_name, desc)
  return function()
    local ok, telescope = pcall(require, "telescope")
    if not ok then
      vim.notify("[git] telescope not available", vim.log.levels.WARN)
      return
    end
    local ext = telescope.extensions and telescope.extensions.git_worktree
    if not ext or type(ext[fn_name]) ~= "function" then
      vim.notify(
        "[git] git_worktree Telescope extension is not loaded — "
          .. desc
          .. " unavailable.\n"
          .. "Check :Lazy log git-worktree.nvim, or run: :lua require('telescope').load_extension('git_worktree')",
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

  -- ── Signs, hunks, blame, stash ─────────────────────────────────────────────
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPost", "BufNewFile" },
    keys = {
      { "<leader>.zz", stash_push, desc = "Git stash push" },
      { "<leader>.zp", stash_pop, desc = "Git stash pop" },
      { "<leader>.zl", "<cmd>Telescope git_stash<cr>", desc = "Git stash list (Telescope)" },
    },
    opts = {
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
      current_line_blame = false,
      on_attach = function(bufnr)
        local gs = require("gitsigns")
        local map = vim.keymap.set
        local function visual_lines()
          return { vim.fn.line("."), vim.fn.line("v") }
        end
        map("n", "]h", gs.next_hunk, { buffer = bufnr, desc = "Next hunk" })
        map("n", "[h", gs.prev_hunk, { buffer = bufnr, desc = "Prev hunk" })
        map("n", "<leader>.p", gs.preview_hunk, { buffer = bufnr, desc = "Preview hunk" })
        map("n", "<leader>.r", gs.reset_hunk, { buffer = bufnr, desc = "Reset hunk" })
        map("n", "<leader>.S", gs.stage_hunk, { buffer = bufnr, desc = "Stage hunk" })
        map("n", "<leader>.B", gs.toggle_current_line_blame, { buffer = bufnr, desc = "Toggle git blame" })
        map("v", "<leader>.r", function()
          gs.reset_hunk(visual_lines())
        end, { buffer = bufnr, desc = "Reset hunk (visual)" })
        map("v", "<leader>.S", function()
          gs.stage_hunk(visual_lines())
        end, { buffer = bufnr, desc = "Stage hunk (visual)" })
      end,
    },
  },

  { "kdheepak/lazygit.nvim", cmd = "LazyGit", dependencies = "nvim-lua/plenary.nvim" },

  -- ── Diff views and file history ────────────────────────────────────────────
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    keys = {
      {
        "<leader>.zh",
        function()
          local file = vim.fn.expand("%:p")
          if file == "" then
            vim.notify("[git] no file in current buffer", vim.log.levels.WARN)
            return
          end
          vim.cmd("DiffviewFileHistory --follow " .. vim.fn.fnameescape(file))
        end,
        desc = "Git file history --follow (rename-tracking)",
      },
    },
    opts = {},
  },

  -- ── Conflict resolution ────────────────────────────────────────────────────
  {
    "akinsho/git-conflict.nvim",
    event = "VeryLazy",
    keys = {
      { "<leader>gco", "<cmd>GitConflictChooseOurs<cr>", desc = "Choose ours" },
      { "<leader>gct", "<cmd>GitConflictChooseTheirs<cr>", desc = "Choose theirs" },
      { "<leader>gcb", "<cmd>GitConflictChooseBoth<cr>", desc = "Choose both" },
      { "<leader>gc0", "<cmd>GitConflictChooseNone<cr>", desc = "Choose neither" },
    },
    opts = {
      default_mappings = false,
      disable_diagnostics = false,
      highlights = { incoming = "DiffAdd", current = "DiffText", ancestor = "DiffChange" },
    },
    config = function(_, opts)
      require("core.util.safe_setup").run("git-conflict", function()
        require("git-conflict").setup(opts)
      end, "run :Lazy update git-conflict.nvim")
    end,
  },

  -- ── GitHub ─────────────────────────────────────────────────────────────────
  {
    "pwntester/octo.nvim",
    cmd = "Octo",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim", "nvim-tree/nvim-web-devicons" },
    keys = {
      { "<leader>.oi", "<cmd>Octo issue list<cr>", desc = "GitHub issues" },
      { "<leader>.op", "<cmd>Octo pr list<cr>", desc = "GitHub PRs" },
      { "<leader>.or", "<cmd>Octo review start<cr>", desc = "Start PR review" },
      { "<leader>.oc", "<cmd>Octo pr checkout<cr>", desc = "Checkout PR" },
    },
    config = function()
      local ok, octo = pcall(require, "octo")
      if not ok then
        vim.notify("[git] octo.nvim is not loadable — run :Lazy", vim.log.levels.WARN)
        return
      end

      local opts = {
        use_local_fs = false,
        default_remote = { "upstream", "origin" },
        picker = "telescope",
        comment_icon = "▎",
        file_panel = { size = 10, use_icons = true },
      }

      -- The timeout is set only when this octo version declares that option. Otherwise a user value is reported.
      local known = true
      local ok_cfg, octo_config = pcall(require, "octo.config")
      if ok_cfg and type(octo_config) == "table" then
        local defaults = type(octo_config.defaults) == "table" and octo_config.defaults
          or (type(octo_config.get_default_values) == "function" and octo_config.get_default_values())
          or {}
        known = defaults.timeout ~= nil
      end

      if known then
        opts.timeout = flags.get("octo_timeout_ms", 10000)
      elseif vim.g.octo_timeout_ms ~= nil then
        vim.notify(
          string.format(
            "[git] vim.g.octo_timeout_ms = %d is set, but the installed octo.nvim\n"
              .. "does not expose a 'timeout' option, so it is not passed.\n"
              .. "Run :Lazy update octo.nvim, or check octo.nvim's current option names.",
            vim.g.octo_timeout_ms
          ),
          vim.log.levels.WARN
        )
      end

      require("core.util.safe_setup").run("octo", function()
        octo.setup(opts)
      end, "run :Lazy update octo.nvim")
    end,
  },

  -- ── Neogit, commit graph and fugitive ─────────────────────────────────────
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
      { "<leader>.N", "<cmd>Neogit<cr>", desc = "Neogit status" },
      { "<leader>.C", "<cmd>Neogit commit<cr>", desc = "Neogit commit" },
    },
    opts = {},
  },
  { "tpope/vim-fugitive", cmd = { "Git", "G" } },
  {
    "junegunn/gv.vim",
    cmd = "GV",
    dependencies = { "tpope/vim-fugitive" },
    keys = { { "<leader>.v", "<cmd>GV<cr>", desc = "Commit graph (GV)" } },
  },

  -- ── Worktrees ──────────────────────────────────────────────────────────────
  {
    "ThePrimeagen/git-worktree.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    event = "VeryLazy",
    keys = {
      {
        "<leader>.wl",
        worktree_action("git_worktrees", "Git worktree list / switch"),
        desc = "Git worktree list / switch",
      },
      { "<leader>.wc", worktree_action("create_git_worktree", "Git worktree create"), desc = "Git worktree create" },
    },
    config = function()
      local safe = require("core.util.safe_setup")
      local wt = require("git-worktree")
      safe.run("git-worktree", function()
        wt.setup()
      end, "run :Lazy update git-worktree.nvim")

      local ok_t, telescope = pcall(require, "telescope")
      if ok_t then
        safe.run("git_worktree extension", function()
          telescope.load_extension("git_worktree")
        end, "check :Lazy log git-worktree.nvim")
      end
    end,
  },
}

-- lua/plugins/specs/editor.lua — editor tooling
--

return {

  -- ── Harpoon ────────────────────────────────────────────────────────────────
  {
    "ThePrimeagen/harpoon", branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    event = "VeryLazy",
    opts = {
      settings = {
        save_on_toggle = true, save_on_change = true, enter_on_select = false,
        key = function()
          local ok, path = pcall(require, "core.util.path")
          if ok then local root = path.find_root(); if root and root ~= "" then return root end end
          return vim.uv.cwd() or ""
        end,
      },
    },
    config = function(_, opts)
      local ok = pcall(function() require("harpoon").setup(opts) end)
      if not ok then vim.notify("harpoon setup failed", vim.log.levels.WARN) end
    end,
  },

  -- ── Telescope ──────────────────────────────────────────────────────────────
  {
    "nvim-telescope/telescope.nvim",
    cmd          = "Telescope",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzf-native.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      local ok = pcall(function()
        require("telescope").setup({
          defaults = {
            layout_strategy  = "horizontal",
            layout_config    = { height = 0.9, preview_cutoff = 120, prompt_position = "bottom" },
            sorting_strategy = "ascending",
            file_ignore_patterns = { "/node_modules/", "/.git/", "/dist/", "/build/", "/.venv/", "/__pycache__/" },
          },
          extensions = { fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true, case_mode = "smart_case" } },
        })
        -- Only load fzf extension if the native library was actually built.
        local ok_fzf = pcall(function() require("telescope").load_extension("fzf") end)
        if not ok_fzf then
          vim.notify(
            "[telescope] fzf-native extension failed to load.\n"
            .. "The native library may not have been compiled. Run:\n"
            .. "  cd ~/.local/share/nvim/lazy/telescope-fzf-native.nvim && make\n"
            .. "Falling back to built-in sorter.",
            vim.log.levels.DEBUG
          )
        end
        pcall(function() require("telescope").load_extension("git_worktree") end)
      end)
      if not ok then vim.notify("telescope setup failed", vim.log.levels.WARN) end
    end,
  },

  -- ── telescope-fzf-native ──────────────────────────────────────────────────
  --
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    lazy  = true,
    build = function()
      if vim.fn.executable("cmake") == 1 then
        return "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
      elseif vim.fn.executable("make") == 1 then
        return "make"
      else
        vim.notify("[telescope-fzf-native] cmake/make unavailable — native sorter will not be built", vim.log.levels.WARN)
        return ""
      end
    end,
    cond = function()
      local plugin_dir = vim.fn.stdpath("data") .. "/lazy/telescope-fzf-native.nvim"
      -- Check for the built shared library under build/.
      local lib_so  = plugin_dir .. "/build/libfzf.so"
      local lib_dyl = plugin_dir .. "/build/libfzf.dylib"
      local lib_dll = plugin_dir .. "/build/libfzf.dll"
      if vim.fn.filereadable(lib_so)  == 1 then return true end
      if vim.fn.filereadable(lib_dyl) == 1 then return true end
      if vim.fn.filereadable(lib_dll) == 1 then return true end
      -- Also accept the root-level build that `make` produces.
      if vim.fn.filereadable(plugin_dir .. "/libfzf.so")  == 1 then return true end
      if vim.fn.filereadable(plugin_dir .. "/libfzf.dylib")== 1 then return true end
      vim.notify(
        "[telescope-fzf-native] compiled library not found — using built-in sorter.\n"
        .. "To build: cd " .. plugin_dir .. " && make",
        vim.log.levels.DEBUG
      )
      return false
    end,
  },

  -- ── Project-scoped Telescope search ────────────────────────────────────────
  {
    "nvim-telescope/telescope.nvim",
    keys = {
      {
        "<leader>fP",
        function()
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local ok, tb = pcall(require, "telescope.builtin")
          if ok then pcall(tb.live_grep, { cwd = root, prompt_title = "Grep: " .. vim.fn.fnamemodify(root, ":~") }) end
        end,
        desc = "Live grep (project root)",
      },
      {
        "<leader>fp",
        function()
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local ok, tb = pcall(require, "telescope.builtin")
          if ok then pcall(tb.find_files, { cwd = root, prompt_title = "Files: " .. vim.fn.fnamemodify(root, ":~") }) end
        end,
        desc = "Find files (project root)",
      },
    },
  },

  -- ── mini.visits — frecency-based file switching ───────────────────────────
  {
    "echasnovski/mini.nvim",
    keys = {
      {
        "<leader>fv",
        function()
          local ok_v, visits = pcall(require, "mini.visits")
          if not ok_v then
            vim.notify("[visits] mini.visits not available", vim.log.levels.WARN)
            return
          end
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()

          local paths = vim.tbl_filter(function(p)
            return vim.startswith(p, root)
          end, visits.list_paths() or {})

          if #paths == 0 then
            vim.notify("[visits] no visits recorded for this project", vim.log.levels.INFO)
            return
          end

          local ok_tb, tb = pcall(require, "telescope.builtin")
          if not ok_tb then
            vim.notify("[visits] telescope not available", vim.log.levels.WARN)
            return
          end

          local tmpfile = vim.fn.tempname()
          if not pcall(vim.fn.writefile, paths, tmpfile) then
            vim.notify("[visits] failed to build frecency file list", vim.log.levels.WARN)
            return
          end

          pcall(tb.find_files, {
            find_command = { "cat", tmpfile },
            prompt_title = "Recent files (frecency)",
            cwd          = root,
          })

          vim.defer_fn(function()
            -- Only delete if the file still exists (not already cleaned up).
            local ok_stat, _ = pcall(vim.uv.fs_stat, tmpfile)
            if ok_stat then pcall(os.remove, tmpfile) end
          end, 30000)
        end,
        desc = "Frequent/recent files (mini.visits)",
      },
    },
  },

  -- ── Spectre ────────────────────────────────────────────────────────────────
  {
    "nvim-pack/nvim-spectre",
    cmd          = "Spectre",
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      color_devicons = true, open_cmd = "noswapfile vnew",
      find_engine  = { rg = { cmd = "rg", args = { "--color=never","--no-heading","--with-filename","--line-number","--column" } } },
      replace_engine = { sed = { cmd = "sed", args = nil } },
    },
  },

  -- ── Todo comments ──────────────────────────────────────────────────────────
  {
    "folke/todo-comments.nvim",
    cmd          = { "TodoTelescope" },
    dependencies = "nvim-lua/plenary.nvim",
    keys = { { "<leader>xT", "<cmd>TodoTelescope<cr>", desc = "Find TODOs" } },
    opts = {
      signs = false,
      keywords = {
        FIX  = { icon = " ", color = "error",   alt = { "FIXME","BUG","FIXIT","ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning" },
        WARN = { icon = " ", color = "warning", alt = { "WARNING","XXX" } },
        PERF = { icon = " ", alt  = { "OPTIM","PERFORMANCE","OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint",    alt = { "INFO" } },
        TEST = { icon = "⚙ ", color = "test",   alt = { "TESTING","PASSED","FAILED" } },
      },
    },
  },

  -- ── Neo-tree ───────────────────────────────────────────────────────────────
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch       = "v3.x",
    cmd          = "Neotree",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-tree/nvim-web-devicons", "MunifTanjim/nui.nvim" },
    keys = {
      { "<leader>ee", "<cmd>Neotree reveal<cr>",  desc = "Toggle explorer"  },
      { "<leader>ef", "<cmd>Neotree focus<cr>",   desc = "Focus explorer"   },
      { "<leader>ec", "<cmd>Neotree close<cr>",   desc = "Close explorer"   },
      { "<leader>er", "<cmd>Neotree refresh<cr>", desc = "Refresh explorer" },
    },
    opts = {
      close_if_last_window = false, popup_border_style = "rounded",
      enable_git_status = true, enable_diagnostics = true,
      filesystem = {
        follow_current_file   = { enabled = true, leave_dirs_open = false },
        hijack_netrw_behavior = "open_current",
        use_libuv             = true,
        filtered_items = {
          visible = false, hide_dotfiles = true, hide_gitignored = true,
          always_show = { ".env", ".gitignore" },
        },
      },
    },
  },

  -- ── Flash ──────────────────────────────────────────────────────────────────
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts  = { modes = { search = { enabled = false }, char = { enabled = false } } },
    config = function(_, opts) pcall(function() require("flash").setup(opts) end) end,
  },

  -- ── vim-tmux-navigator ────────────────────────────────────────────────────
  {
    "christoomey/vim-tmux-navigator",
    cmd = { "TmuxNavigateLeft","TmuxNavigateDown","TmuxNavigateUp","TmuxNavigateRight" },
    init = function()
      vim.g.tmux_navigator_no_mappings   = 1
      vim.g.tmux_navigator_no_wrap       = 1
      vim.g.tmux_navigator_preserve_zoom = 1
    end,
    keys = {
      { "<C-h>", "<cmd>TmuxNavigateLeft<cr>",  mode = "n", noremap = true, silent = true, desc = "Navigate left  (split/pane)" },
      { "<C-j>", "<cmd>TmuxNavigateDown<cr>",  mode = "n", noremap = true, silent = true, desc = "Navigate down  (split/pane)" },
      { "<C-k>", "<cmd>TmuxNavigateUp<cr>",    mode = "n", noremap = true, silent = true, desc = "Navigate up    (split/pane)" },
      { "<C-l>", "<cmd>TmuxNavigateRight<cr>", mode = "n", noremap = true, silent = true, desc = "Navigate right (split/pane)" },
    },
  },

  -- ── Sessions ───────────────────────────────────────────────────────────────
  {
    "folke/persistence.nvim",
    event = "BufReadPre",
    config = function()
      require("persistence").setup({ dir = vim.fn.stdpath("state") .. "/sessions/", options = vim.opt.sessionoptions:get() })
    end,
    keys = {
      { "<leader>qs", function() require("persistence").load() end,                desc = "Restore session"      },
      { "<leader>ql", function() require("persistence").load({ last = true }) end, desc = "Restore last session" },
      { "<leader>qd", function() require("persistence").stop() end,                desc = "Stop session saving"  },
    },
  },

  -- ── Template ───────────────────────────────────────────────────────────────
  {
    "nvimdev/template.nvim",
    cmd = "Template",
    config = function()
      local ok, template = pcall(require, "template")
      if not ok then return end
      local function git_cfg(key)
        local val = vim.fn.system("git config " .. key):gsub("\n", "")
        return (val ~= "" and not val:find("^fatal")) and val or nil
      end
      pcall(function()
        template.setup({
          temp_dir     = vim.fn.stdpath("config") .. "/templates",
          auto_name    = false,
          author_name  = git_cfg("user.name")  or "Your Name",
          author_email = git_cfg("user.email") or "your.email@example.com",
          web_devicons = true,
        })
      end)
    end,
  },
}

-- lua/plugins/specs/lang/database.lua — database UI and SQL completion
--

local shared = require("plugins.specs.lang.shared")

-- ── DATABASE_URL auto-load ────────────────────────────────────────────────
--
-- Priority:
--   1. DATABASE_URL env var (already exported in the shell)
--   2. DB_URL env var (common alternate name)
--   3. .env file at the project root (first matching KEY=value line)

local function load_database_url()
  -- Respect an explicit user override; don't clobber it.
  if vim.g.db and vim.g.db ~= "" then return end

  -- 1 & 2: environment variables
  for _, env_key in ipairs({ "DATABASE_URL", "DB_URL", "DATABASE_URI" }) do
    local val = os.getenv(env_key)
    if val and val ~= "" then
      vim.g.db = val
      vim.notify(
        string.format("[database] Connection loaded from $%s", env_key),
        vim.log.levels.INFO
      )
      return
    end
  end

  -- 3: .env file at project root
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return end

  local env_files = { root .. "/.env", root .. "/.env.local", root .. "/.env.development" }
  for _, f in ipairs(env_files) do
    if vim.fn.filereadable(f) ~= 1 then goto continue end

    local ok_r, lines = pcall(vim.fn.readfile, f)
    if not ok_r then goto continue end

    for _, line in ipairs(lines) do
      local key, val = line:match("^%s*([%w_]+)%s*=%s*(.+)%s*$")
      if key and vim.tbl_contains({ "DATABASE_URL", "DB_URL", "DATABASE_URI" }, key) then
        -- Strip surrounding quotes if present.
        val = val:gsub('^["\']', ""):gsub('["\']$', "")
        if val ~= "" then
          vim.g.db = val
          vim.notify(
            string.format("[database] Connection loaded from %s (%s)",
              vim.fn.fnamemodify(f, ":~:."), key),
            vim.log.levels.INFO
          )
          return
        end
      end
    end

    ::continue::
  end
end

-- Run once when dadbod-ui is first opened, and again on DirChanged.
vim.api.nvim_create_autocmd({ "User" }, {
  pattern  = "DBUIOpening",
  group    = vim.api.nvim_create_augroup("DatabaseUrlLoad", { clear = true }),
  callback = function() vim.schedule(load_database_url) end,
  desc     = "Auto-load DATABASE_URL before DBUI opens",
})

vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("DatabaseUrlReload", { clear = true }),
  callback = function()
    -- Only reload if the user hasn't set a manual connection.
    if not vim.g.db or vim.g.db == "" then
      vim.schedule(load_database_url)
    end
  end,
})

return {
  -- ── vim-dadbod-ui ──────────────────────────────────────────────────────────
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      "tpope/vim-dadbod",
      "kristijanhusak/vim-dadbod-completion",
    },
    cmd  = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
    keys = {
      { "<leader>dbu", "<cmd>DBUIToggle<cr>",        desc = "DB UI Toggle"      },
      { "<leader>dba", "<cmd>DBUIAddConnection<cr>", desc = "DB Add Connection" },
      { "<leader>dbf", "<cmd>DBUIFindBuffer<cr>",    desc = "DB Find Buffer"    },
      {
        "<leader>dbs",
        function()
          -- Schema introspection:
          local schema_query = table.concat({
            "-- Schema introspection",
            "-- PostgreSQL",
            "SELECT table_name, table_type",
            "  FROM information_schema.tables",
            " WHERE table_schema = 'public'",
            " ORDER BY table_name;",
            "",
            "-- MySQL / MariaDB",
            "-- SHOW TABLES;",
            "",
            "-- SQLite",
            "-- SELECT name FROM sqlite_master WHERE type='table';",
          }, "\n")

          local buf = vim.api.nvim_create_buf(false, true)
          vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(schema_query, "\n"))
          vim.bo[buf].filetype  = "sql"
          vim.bo[buf].buftype   = "nofile"
          vim.bo[buf].swapfile  = false
          vim.bo[buf].buflisted = false
          vim.api.nvim_buf_set_name(buf, "schema-introspection.sql")
          vim.api.nvim_set_current_buf(buf)
          vim.notify(
            "[database] Schema query opened — edit and run with <leader>dbe (DB execute)",
            vim.log.levels.INFO
          )
        end,
        desc = "DB Schema Introspection (scratch buffer)",
      },
      {
        "<leader>dbc",
        function()
          -- Clear the auto-loaded connection so the user can set a new one.
          vim.g.db = ""
          vim.notify("[database] Connection cleared — reopen DBUI to reconnect",
            vim.log.levels.INFO)
        end,
        desc = "DB Clear Connection",
      },
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts           = 1
      vim.g.db_ui_save_location            = vim.fn.stdpath("data") .. "/dadbod-ui"
      vim.g.db_ui_show_database_icon       = 1
      vim.g.db_ui_force_echo_notifications = 1
      vim.g.db_ui_win_position             = "left"
      vim.g.db_ui_winwidth                 = 40

      vim.schedule(load_database_url)
    end,
  },

  -- ── vim-dadbod-completion ──────────────────────────────────────────────────

  {
    "kristijanhusak/vim-dadbod-completion",
    ft     = { "sql", "mysql", "plsql" },
    config = function()
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("DadbodCompletion", { clear = true }),
        pattern  = { "sql", "mysql", "plsql" },
        callback = function()
          pcall(function()
            vim.opt_local.omnifunc = "vim_dadbod_completion#omni"
          end)
        end,
      })
    end,
  },

  shared.treesitter({ "sql" }),
}

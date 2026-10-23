-- lua/plugins/specs/lang/database.lua — database UI, SQL execution and completion wiring
--
-- Connection resolution lives in core.util.dburl. This file wires keymaps, autocmds and the
-- schema scratch buffer. Depends on vim-dadbod (:DB, b:db/g:db), vim-dadbod-ui (DBUI*) and
-- vim-dadbod-completion (omnifunc; blink.cmp uses its own provider).

local shared = require("plugins.specs.lang.shared")
local dburl  = require("core.util.dburl")

local function sync_here()
  dburl.sync(dburl.project_root(0))
end

local function with_connection(ex_cmd)
  return function()
    sync_here()
    vim.cmd(ex_cmd)
  end
end

local function open_schema_scratch()
  local url = vim.g.db
  if type(url) ~= "string" or url == "" then
    vim.notify("[database] no connection for this project — open <leader>dbu or set vim.g.db", vim.log.levels.WARN)
    return
  end
  local query = dburl.schema_query(url)
  if not query then
    local scheme = url:match("^(%a[%w+.-]*):") or "?"
    vim.notify(("[database] no built-in schema query for '%s' — browse it with <leader>dbu"):format(scheme),
      vim.log.levels.WARN)
    return
  end
  local header = ("-- Schema introspection (%s). Run it with :%%DB\n"):format(dburl.dialect(url))
  local buf = vim.api.nvim_create_buf(false, true)
  local ok, err = pcall(function()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(header .. query, "\n", { plain = true }))
    vim.bo[buf].buftype = "nofile"
    vim.bo[buf].bufhidden = "wipe"
    vim.bo[buf].swapfile = false
    vim.bo[buf].filetype = "sql"
    vim.b[buf].db = url   -- :%DB keeps targeting this connection even if g:db changes later
    vim.api.nvim_buf_set_name(buf, ("schema-introspection#%d.sql"):format(buf))
  end)
  if not ok then
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
    vim.notify("[database] could not create the schema buffer: " .. tostring(err), vim.log.levels.ERROR)
    return
  end
  vim.api.nvim_set_current_buf(buf)
  vim.notify("[database] schema query ready — run it with :%DB", vim.log.levels.INFO)
end

local function clear_connection()
  local ok, note = dburl.clear(dburl.project_root(0))
  if not ok then
    vim.notify("[database] clear failed: " .. tostring(note), vim.log.levels.ERROR)
    return
  end
  vim.notify("[database] connection cleared for this project" .. (note and (" — " .. note) or ""),
    vim.log.levels.INFO)
end

-- File scope, so the omnifunc exists before the first SQL buffer gets typed in.
vim.api.nvim_create_autocmd("FileType", {
  group    = vim.api.nvim_create_augroup("DadbodOmnifunc", { clear = true }),
  pattern  = { "sql", "mysql", "plsql" },
  callback = function(e) vim.bo[e.buf].omnifunc = "vim_dadbod_completion#omni" end,
  desc     = "Use vim-dadbod-completion as the omnifunc for SQL buffers",
})

vim.api.nvim_create_autocmd("DirChanged", {
  group    = vim.api.nvim_create_augroup("DatabaseUrlSync", { clear = true }),
  callback = function() vim.schedule(sync_here) end,
  desc     = "Re-resolve the database connection when the working directory changes",
})

return {
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = { "tpope/vim-dadbod", "kristijanhusak/vim-dadbod-completion" },
    cmd  = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
    keys = {
      { "<leader>dbu", with_connection("DBUIToggle"),        desc = "DB UI Toggle" },
      { "<leader>dba", with_connection("DBUIAddConnection"), desc = "DB Add Connection" },
      { "<leader>dbf", with_connection("DBUIFindBuffer"),    desc = "DB Find Buffer" },
      { "<leader>dbs", open_schema_scratch,                  desc = "DB Schema Introspection (scratch buffer)" },
      { "<leader>dbc", clear_connection,                     desc = "DB Clear Connection" },
    },
    init = function()
      -- g:db_ui_show_database_icon was removed: documented upstream but never read by the code.
      vim.g.db_ui_use_nerd_fonts           = 1
      vim.g.db_ui_save_location            = vim.fn.stdpath("data") .. "/dadbod-ui"
      vim.g.db_ui_force_echo_notifications = 1
      vim.g.db_ui_win_position             = "left"
      vim.g.db_ui_winwidth                 = 40
      vim.schedule(sync_here)
    end,
  },

  {
    "kristijanhusak/vim-dadbod-completion",
    ft = { "sql", "mysql", "plsql" },
  },

  shared.treesitter({ "sql" }),
}

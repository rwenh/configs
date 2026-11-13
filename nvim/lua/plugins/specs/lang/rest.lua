-- lua/plugins/specs/lang/rest.lua — REST client (kulala.nvim)
--

local shared = require("plugins.specs.lang.shared")
local platform = require("core.util.platform")

local function notify(msg, level)
  vim.notify("[rest] " .. msg, level or vim.log.levels.WARN)
end

-- kulala replaces any body larger than ui.max_response_size (32 KB by default) with this note.
local TRUNCATION_NOTE = "The size of response is >"

-- ── Dispatch ──────────────────────────────────────────────────────────────────

---@param fn_name string
---@param ... any
local function kulala_call(fn_name, ...)
  local ok_k, kulala = pcall(require, "kulala")
  if not ok_k then
    notify("kulala.nvim is not loaded — run :Lazy")
    return
  end
  local fn = kulala[fn_name]
  if type(fn) ~= "function" then
    notify("kulala." .. fn_name .. " is not available — update kulala.nvim (:Lazy update kulala.nvim)")
    return
  end
  local ok, err = pcall(fn, ...)
  if not ok then
    notify(fn_name .. " failed: " .. tostring(err), vim.log.levels.ERROR)
  end
end

-- ── Auth tokens ───────────────────────────────────────────────────────────────
-- A token is never written into a buffer or a file. It lives in this process's environment as
-- NVIM_REST_<NAME>; the request refers to it with kulala's {{$processEnv ...}} placeholder.
-- Verify that your kulala version supports $processEnv (see its README).
local TOKEN_PREFIX = "NVIM_REST_"
local _token_names = {} -- name -> true

local function token_var(name)
  return TOKEN_PREFIX .. (name:upper():gsub("[^%w_]", "_"))
end

local function store_token(name, value)
  vim.fn.setenv(token_var(name), value)
  _token_names[name] = true
end

for name, value in pairs(type(vim.g.rest_auth_tokens) == "table" and vim.g.rest_auth_tokens or {}) do
  if type(value) == "string" then
    store_token(name, value)
  end
end

local function pick_token(callback)
  local names = vim.tbl_keys(_token_names)
  if #names == 0 then
    notify("No auth tokens stored. Add one with <leader>reA.", vim.log.levels.INFO)
    return
  end
  table.sort(names)
  vim.ui.select(names, { prompt = "Select auth token:" }, function(choice)
    if choice then
      callback(choice)
    end
  end)
end

local function add_token()
  vim.ui.input({ prompt = "Token name (letters, digits, _): " }, function(name)
    name = vim.trim(name or "")
    if not name:match("^[%w_]+$") then
      if name ~= "" then
        notify("name must be letters, digits or _")
      end
      return
    end
    vim.ui.input({ prompt = "Token value (e.g. Bearer abc123): " }, function(value)
      if not value or value == "" then
        return
      end
      store_token(name, value)
      notify("Token '" .. name .. "' stored for this session only", vim.log.levels.INFO)
    end)
  end)
end

local function insert_token()
  pick_token(function(name)
    local row = vim.api.nvim_win_get_cursor(0)[1]
    vim.api.nvim_buf_set_lines(0, row, row, false, { "Authorization: {{$processEnv " .. token_var(name) .. "}}" })
  end)
end

-- ── Response diff ─────────────────────────────────────────────────────────────

---@return table[]
local function history()
  local ok, db = pcall(require, "kulala.db")
  local list = (ok and type(db) == "table" and db.global_data and db.global_data.responses) or {}
  local out = {}
  for i = #list, 1, -1 do
    out[#out + 1] = list[i]
  end
  return out
end

local function is_truncated(response)
  return type(response.body) == "string" and response.body:sub(1, #TRUNCATION_NOTE) == TRUNCATION_NOTE
end

local function scratch(name, body)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"
  local text = body or ""
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
  vim.bo[buf].filetype = text:match("^%s*[%[{]") and "json" or "text"
  vim.bo[buf].modifiable = false
  pcall(vim.api.nvim_buf_set_name, buf, string.format("%s#%d", name, buf))
  return buf
end

local function diff_responses(newer, older)
  vim.cmd("tabnew")
  local win_a = vim.api.nvim_get_current_win()
  -- tabnew shows an empty placeholder buffer; have it wiped once it is replaced.
  vim.bo[vim.api.nvim_win_get_buf(win_a)].bufhidden = "wipe"
  vim.api.nvim_win_set_buf(win_a, scratch("response-latest", newer.body))

  vim.cmd("vsplit")
  local win_b = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win_b, scratch("response-previous", older.body))

  for _, w in ipairs({ win_a, win_b }) do
    vim.api.nvim_win_call(w, function()
      vim.cmd("diffthis")
    end)
  end

  local aug = vim.api.nvim_create_augroup("RestDiffCleanup_" .. win_a, { clear = true })
  vim.api.nvim_create_autocmd("WinClosed", {
    group = aug,
    pattern = { tostring(win_a), tostring(win_b) },
    once = true,
    callback = function()
      vim.schedule(function()
        for _, w in ipairs({ win_a, win_b }) do
          if vim.api.nvim_win_is_valid(w) then
            pcall(vim.api.nvim_win_call, w, function()
              vim.cmd("diffoff")
            end)
          end
        end
      end)
      pcall(vim.api.nvim_del_augroup_by_id, aug)
    end,
    desc = "Turn diff mode off when a REST diff window closes",
  })
end

local function pick_and_diff()
  local list = history()
  if #list < 2 then
    notify("Need at least 2 responses to diff", vim.log.levels.INFO)
    return
  end

  local items = {}
  for i, r in ipairs(list) do
    local label = (r.name and r.name ~= "" and r.name) or r.url or "request"
    items[#items + 1] = string.format(
      "[%d] %s  %s  %s",
      i,
      label,
      tostring(r.response_code or "?"),
      r.duration and (r.duration .. "ms") or ""
    )
  end

  vim.ui.select(items, { prompt = "Diff latest against:" }, function(_, idx)
    if not idx or idx == 1 then
      return
    end
    local newer, older = list[1], list[idx]
    if is_truncated(newer) or is_truncated(older) then
      notify(
        "Response too large to diff: kulala keeps only a note for bodies over ui.max_response_size "
          .. "(32 KB by default). Raise that option in the kulala setup and re-run the requests."
      )
      return
    end
    diff_responses(newer, older)
  end)
end

-- ── Keymaps ───────────────────────────────────────────────────────────────────

local function http_key(lhs, rhs, desc)
  return { lhs, rhs, desc = desc, ft = "http" }
end

return {
  {
    "mistweaverco/kulala.nvim",
    ft = { "http", "rest" },
    version = "*",

    init = function()
      pcall(vim.filetype.add, { extension = { rest = "http" } })
      -- lazy.nvim runs init at startup, so the prerequisite checks wait for the first .http buffer.
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "http", "rest" },
        once = true,
        group = vim.api.nvim_create_augroup("KulalaPrereqs", { clear = true }),
        callback = function()
          if vim.fn.executable("curl") ~= 1 then
            notify("curl not found — kulala.nvim will not function.\nInstall: " .. platform.install_hint("curl"))
          end
          if vim.fn.executable("tree-sitter") ~= 1 then
            notify(
              "tree-sitter CLI not found — kulala.nvim builds its HTTP parser with it "
                .. "(treesitter.cli_path), so .http buffers may fail to load.\n"
                .. "Install: "
                .. platform.install_hint("tree-sitter")
            )
          end
        end,
      })
    end,

    opts = {
      default_env = "dev",
      environment_scope = "b",
      ui = { split_direction = "vertical" },
    },

    config = function(_, opts)
      -- kulala builds its HTTP parser with the tree-sitter CLI and raises an uncaught error during setup
      -- without it. Skip setup in that case; the FileType notice in init explains what is missing.
      if vim.fn.executable("tree-sitter") ~= 1 then
        return
      end
      local ok, err = pcall(function()
        require("kulala").setup(opts)
      end)
      if not ok then
        notify("kulala.nvim setup failed: " .. tostring(err) .. "\nRun :Lazy update kulala.nvim")
      end
    end,

    keys = {
      http_key("<leader>rer", function()
        kulala_call("run")
      end, "REST run request"),
      http_key("<leader>rel", function()
        kulala_call("replay")
      end, "REST run last"),
      http_key("<leader>rep", function()
        kulala_call("inspect")
      end, "REST preview (inspect curl)"),
      http_key("<leader>ree", function()
        kulala_call("set_selected_env")
      end, "REST select environment"),
      http_key("<leader>ren", function()
        kulala_call("jump_next")
      end, "REST jump to next request"),
      http_key("<leader>reN", function()
        kulala_call("jump_prev")
      end, "REST jump to previous request"),
      http_key("<leader>rec", function()
        kulala_call("copy")
      end, "REST copy as curl"),
      http_key("<leader>red", pick_and_diff, "REST diff responses"),
      http_key("<leader>reA", add_token, "REST add auth token"),
      http_key("<leader>reat", insert_token, "REST insert auth token below cursor"),
    },
  },

  shared.treesitter({ "http", "json" }),
}

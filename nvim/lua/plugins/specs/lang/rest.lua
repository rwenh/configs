-- lua/plugins/specs/lang/rest.lua — REST client (kulala.nvim)
--

local shared = require("plugins.specs.lang.shared")

local _tokens = type(vim.g.rest_auth_tokens) == "table"
  and vim.tbl_deep_extend("keep", {}, vim.g.rest_auth_tokens)
  or {}

local function pick_token(callback)
  local names = vim.tbl_keys(_tokens)
  if #names == 0 then
    vim.notify("[rest] No auth tokens stored. Add one with <leader>reA.", vim.log.levels.INFO)
    return
  end
  table.sort(names)
  vim.ui.select(names, { prompt = "Select auth token:" }, function(choice)
    if choice then callback(_tokens[choice]) end
  end)
end

---@return table[]
local function history()
  local ok, db = pcall(require, "kulala.db")
  local list = ok and db.global_data and db.global_data.responses or {}
  local out = {}
  for i = #list, 1, -1 do out[#out + 1] = list[i] end
  return out
end

local function scratch(name, body)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].bufhidden = "wipe"          -- frees the name when its window closes (no E95 next time)
  vim.bo[buf].filetype  = "json"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(body or "", "\n", { plain = true }))
  -- Unique suffix: a name can still collide while an old diff tab is open.
  pcall(vim.api.nvim_buf_set_name, buf, string.format("%s#%d", name, buf))
  return buf
end

local function diff_responses(newer, older)
  vim.cmd("tabnew")
  local win_a = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win_a, scratch("response-latest", newer.body))
  vim.cmd("vsplit")
  local win_b = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(win_b, scratch("response-previous", older.body))
  for _, w in ipairs({ win_a, win_b }) do
    vim.api.nvim_win_call(w, function() vim.cmd("diffthis") end)
  end

  local aug = vim.api.nvim_create_augroup("RestDiffCleanup_" .. win_a, { clear = true })
  vim.api.nvim_create_autocmd("WinClosed", {
    group    = aug,
    pattern  = { tostring(win_a), tostring(win_b) },
    once     = true,
    callback = function()
      vim.schedule(function()
        for _, w in ipairs({ win_a, win_b }) do
          if vim.api.nvim_win_is_valid(w) then pcall(vim.api.nvim_win_call, w, function() vim.cmd("diffoff") end) end
        end
      end)
      pcall(vim.api.nvim_del_augroup_by_id, aug)
    end,
    desc = "Clear diffmode when REST diff windows close",
  })
end

return {
  {
    "mistweaverco/kulala.nvim",
    ft      = { "http", "rest" },
    version = "*",

    init = function()
      pcall(vim.filetype.add, { extension = { rest = "http" } })
      if vim.fn.executable("curl") ~= 1 then
        vim.notify("[rest] curl not found — kulala.nvim will not function.\nInstall: sudo zypper in curl", vim.log.levels.WARN)
      end
    end,

    opts = {
      default_env       = "dev",
      environment_scope = "b",
      ui = { split_direction = "vertical" },
    },

    config = function(_, opts)
      local ok, err = pcall(function() require("kulala").setup(opts) end)
      if not ok then
        vim.notify("[rest] kulala.nvim setup failed: " .. tostring(err) .. "\nRun :Lazy update kulala.nvim", vim.log.levels.WARN)
      end
    end,

    keys = {
      { "<leader>rer", function() pcall(function() require("kulala").run()     end) end, desc = "REST Run Request",            ft = "http" },
      { "<leader>rel", function() pcall(function() require("kulala").replay()  end) end, desc = "REST Run Last",               ft = "http" },
      { "<leader>rep", function() pcall(function() require("kulala").inspect() end) end, desc = "REST Preview (inspect curl)", ft = "http" },
      { "<leader>ree", function() pcall(function() require("kulala").set_selected_env() end) end, desc = "REST Select Env",   ft = "http" },
      { "<leader>ren", function() pcall(function() require("kulala").jump_next() end) end, desc = "REST Jump next request",   ft = "http" },
      { "<leader>reN", function() pcall(function() require("kulala").jump_prev() end) end, desc = "REST Jump prev request",   ft = "http" },
      { "<leader>rec", function() pcall(function() require("kulala").copy()    end) end, desc = "REST Copy as curl",           ft = "http" },

      {
        "<leader>red",
        function()
          local list = history()
          if #list < 2 then
            vim.notify("[rest] Need at least 2 responses to diff", vim.log.levels.INFO)
            return
          end
          local items = {}
          for i, r in ipairs(list) do
            items[#items + 1] = string.format("[%d] %s  %s  %s", i, r.name or r.url or "request",
              tostring(r.response_code or "?"), r.duration and (r.duration .. "ms") or "")
          end
          vim.ui.select(items, { prompt = "Diff latest against:" }, function(_, idx)
            if not idx or idx == 1 then return end
            diff_responses(list[1], list[idx])
          end)
        end,
        desc = "REST Diff responses",
        ft   = "http",
      },

      {
        "<leader>reA",
        function()
          vim.ui.input({ prompt = "Token name: " }, function(name)
            if not name or name == "" then return end
            vim.ui.input({ prompt = "Token value (e.g. Bearer abc123): " }, function(val)
              if not val or val == "" then return end
              _tokens[name] = val
              vim.notify("[rest] Token '" .. name .. "' stored (session only)", vim.log.levels.INFO)
            end)
          end)
        end,
        desc = "REST Add auth token",
        ft   = "http",
      },
      {
        "<leader>reat",
        function()
          pick_token(function(token)
            local row = vim.api.nvim_win_get_cursor(0)[1]
            vim.api.nvim_buf_set_lines(0, row, row, false, { "Authorization: " .. token })
          end)
        end,
        desc = "REST Insert auth token below cursor",
        ft   = "http",
      },
    },
  },

  shared.treesitter({ "http", "json" }),
}

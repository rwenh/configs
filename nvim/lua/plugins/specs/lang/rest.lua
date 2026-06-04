-- lua/plugins/specs/lang/rest.lua — REST client (rest.nvim v3)
--

local shared = require("plugins.specs.lang.shared")

-- ── Response history store ─────────────────────────────────────────────────
-- Persists the last N responses in memory for diffing.
local _response_history = {}   -- { { timestamp, status, body } }
local MAX_HISTORY       = 10

local function store_response(status, body)
  table.insert(_response_history, 1, {
    timestamp = os.date("%H:%M:%S"),
    status    = status,
    body      = body,
  })
  if #_response_history > MAX_HISTORY then
    _response_history[MAX_HISTORY + 1] = nil
  end
end

-- ── Auth token manager ─────────────────────────────────────────────────────
-- Structure: { ["token_name"] = "Bearer abc123", ... }
local _tokens = type(vim.g.rest_auth_tokens) == "table"
  and vim.tbl_deep_extend("keep", {}, vim.g.rest_auth_tokens)
  or {}

local function pick_token(callback)
  local names = vim.tbl_keys(_tokens)
  if #names == 0 then
    vim.notify("[rest] No auth tokens stored. Add one with <leader>reA.", vim.log.levels.INFO)
    return
  end
  vim.ui.select(names, { prompt = "Select auth token:" }, function(choice)
    if choice then callback(_tokens[choice]) end
  end)
end

return {
  {
    "rest-nvim/rest.nvim",
    ft           = "http",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },

    init = function()
      pcall(function()
        vim.filetype.add({ extension = { rest = "http" } })
      end)
    end,

    opts = (function()
      if vim.fn.executable("curl") ~= 1 then
        vim.notify("[rest] curl not found — rest.nvim will not function.\nInstall: sudo zypper in curl", vim.log.levels.WARN)
        return {}
      end
      local env_file = ".env"
      return {
        client       = "curl",
        env_file     = env_file,
        env_pattern  = vim.pesc(env_file) .. "$",
        env_edit_command = "tabedit",
        skip_ssl_verification    = false,
        custom_dynamic_variables = {},
        logs = { level = "info", save = true },
        result = {
          split = { horizontal = false, in_place = false, stay_at_current_window_after_split = true },
          behavior = {
            decode_url = true,
            show_info  = { url = true, headers = true, http_info = true, curl_command = true },
            statistics = {
              enable = true,
              stats  = { { "total_time", "Time taken:" }, { "size_download_t", "Download size:" } },
            },
            formatters = {
              json = vim.fn.executable("jq") == 1 and "jq" or nil,
              html = { cmd = { "prettier", "--parser", "html" } },
            },
          },
        },
        highlight = { enable = true, timeout = 750 },
        request   = { hooks = { encode_url = true, user_agent = "rest.nvim", set_content_type = true } },
      }
    end)(),

    config = function(_, opts)
      if vim.tbl_isempty(opts) then return end
      local ok, err = pcall(function() require("rest-nvim").setup(opts) end)
      if not ok then
        vim.notify("[rest] rest.nvim setup failed: " .. tostring(err) .. "\nRun :Lazy update rest.nvim", vim.log.levels.WARN)
        return
      end

      -- Hook response capture for diffing.
      pcall(function()
        local events = require("rest-nvim.events")
        events.on("ResponseReceived", function(response)
          store_response(
            tostring(response and response.status or "?"),
            response and response.body or ""
          )
        end)
      end)
    end,

    keys = {
      { "<leader>rer", "<cmd>Rest run<cr>",        desc = "REST Run Request",   ft = "http" },
      { "<leader>rel", "<cmd>Rest run last<cr>",   desc = "REST Run Last",      ft = "http" },
      { "<leader>rep", "<cmd>Rest preview<cr>",    desc = "REST Preview",       ft = "http" },
      { "<leader>ree", "<cmd>Rest env select<cr>", desc = "REST Select Env",    ft = "http" },

      -- ── Response diff ─────────────────────────────────────────────────────
      {
        "<leader>red",
        function()
          if #_response_history < 2 then
            vim.notify("[rest] Need at least 2 responses to diff", vim.log.levels.INFO); return
          end
          local items = {}
          for i, r in ipairs(_response_history) do
            table.insert(items, string.format("[%d] %s  %s", i, r.timestamp, r.status))
          end
          vim.ui.select(items, { prompt = "Diff against (compare with most recent):" }, function(_, idx)
            if not idx or idx == 1 then return end
            local a = _response_history[1].body
            local b = _response_history[idx].body
            -- Open two scratch buffers in a vertical split for diff.
            local ba = vim.api.nvim_create_buf(false, true)
            local bb = vim.api.nvim_create_buf(false, true)
            vim.api.nvim_buf_set_lines(ba, 0, -1, false, vim.split(a, "\n"))
            vim.api.nvim_buf_set_lines(bb, 0, -1, false, vim.split(b, "\n"))
            vim.bo[ba].filetype = "json"; vim.bo[bb].filetype = "json"
            vim.api.nvim_buf_set_name(ba, "response-latest")
            vim.api.nvim_buf_set_name(bb, string.format("response-%s", _response_history[idx].timestamp))
            vim.cmd("tabnew")
            vim.api.nvim_set_current_buf(ba)
            vim.cmd("vsplit")
            vim.api.nvim_set_current_buf(bb)
            vim.cmd("windo diffthis")
          end)
        end,
        desc = "REST Diff responses",
        ft   = "http",
      },

      -- ── Auth token manager ────────────────────────────────────────────────
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
            -- Insert token as Authorization header at cursor.
            local row = vim.api.nvim_win_get_cursor(0)[1]
            vim.api.nvim_buf_set_lines(0, row, row, false,
              { "Authorization: " .. token })
          end)
        end,
        desc = "REST Insert auth token at cursor",
        ft   = "http",
      },
    },
  },

  shared.treesitter({ "http", "json" }),
}

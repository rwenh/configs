-- lua/plugins/specs/lang/rest.lua — REST client (kulala.nvim)
--

local shared = require("plugins.specs.lang.shared")

local _response_history = {}
local MAX_HISTORY       = 10
local _kulala_api_warned = false   -- emit the version-mismatch warning only once

local function store_response(status, body)
  table.insert(_response_history, 1, {
    timestamp = os.date("%H:%M:%S"),
    status    = status,
    body      = body,
  })
  if #_response_history > MAX_HISTORY then
    table.remove(_response_history)
  end
end

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
    "mistweaverco/kulala.nvim",
    ft      = { "http", "rest" },
    version = "*",

    init = function()
      pcall(function()
        vim.filetype.add({ extension = { rest = "http" } })
      end)

      if vim.fn.executable("curl") ~= 1 then
        vim.notify(
          "[rest] curl not found — kulala.nvim will not function.\n"
          .. "Install: sudo zypper in curl",
          vim.log.levels.WARN
        )
      end
    end,

    opts = {
      formatters = {
        json = vim.fn.executable("jq") == 1 and { "jq", "." } or { "cat" },
        html = { "prettier", "--parser", "html" },
      },
      show_headers      = true,
      show_stats        = true,
      split_direction   = "vertical",
      default_env       = "dev",
      environment_scope = "b",
      contenttypes = {
        ["application/json"] = { ft = "json", formatter = "json" },
        ["application/xml"]  = { ft = "xml",  formatter = nil    },
        ["text/html"]        = { ft = "html", formatter = "html" },
        ["text/plain"]       = { ft = "text", formatter = nil    },
      },
    },

    config = function(_, opts)
      local ok, err = pcall(function() require("kulala").setup(opts) end)
      if not ok then
        vim.notify(
          "[rest] kulala.nvim setup failed: " .. tostring(err)
          .. "\nRun :Lazy update kulala.nvim",
          vim.log.levels.WARN
        )
        return
      end

      vim.api.nvim_create_autocmd("User", {
        pattern  = "KulalaRequestComplete",
        group    = vim.api.nvim_create_augroup("KulalaHistory", { clear = true }),
        callback = function()
          local ok_k, k = pcall(require, "kulala")
          if not ok_k then return end

          if type(k.get_last_response) ~= "function" then
            if not _kulala_api_warned then
              _kulala_api_warned = true
              vim.notify(
                "[rest] kulala.get_last_response() not found in the installed version.\n"
                .. "Response history and diff are unavailable.\n"
                .. "Update kulala.nvim: :Lazy update kulala.nvim",
                vim.log.levels.WARN
              )
            end
            return
          end

          pcall(function()
            local response = k.get_last_response()
            if response then
              store_response(
                tostring(response.status or "?"),
                response.body or ""
              )
            end
          end)
        end,
        desc = "Store kulala response in history for diff",
      })
    end,

    keys = {
      { "<leader>rer", function() pcall(function() require("kulala").run()     end) end, desc = "REST Run Request",              ft = "http" },
      { "<leader>rel", function() pcall(function() require("kulala").replay()  end) end, desc = "REST Run Last",                 ft = "http" },
      { "<leader>rep", function() pcall(function() require("kulala").inspect() end) end, desc = "REST Preview (inspect curl)",   ft = "http" },
      { "<leader>ree", function() pcall(function() require("kulala").set_selected_env() end) end, desc = "REST Select Env",     ft = "http" },
      { "<leader>ren", function() pcall(function() require("kulala").jump_next() end) end, desc = "REST Jump next request",     ft = "http" },
      { "<leader>reN", function() pcall(function() require("kulala").jump_prev() end) end, desc = "REST Jump prev request",     ft = "http" },
      { "<leader>rec", function() pcall(function() require("kulala").copy()    end) end, desc = "REST Copy as curl",             ft = "http" },

      {
        "<leader>red",
        function()
          if #_response_history < 2 then
            vim.notify("[rest] Need at least 2 responses to diff", vim.log.levels.INFO)
            return
          end
          local items = {}
          for i, r in ipairs(_response_history) do
            table.insert(items, string.format("[%d] %s  %s", i, r.timestamp, r.status))
          end
          vim.ui.select(
            items,
            { prompt = "Diff against (compare with most recent):" },
            function(_, idx)
              if not idx or idx == 1 then return end
              local a = _response_history[1].body
              local b = _response_history[idx].body

              vim.cmd("tabnew")
              local ba = vim.api.nvim_create_buf(false, true)
              local bb = vim.api.nvim_create_buf(false, true)
              vim.api.nvim_buf_set_lines(ba, 0, -1, false, vim.split(a, "\n"))
              vim.api.nvim_buf_set_lines(bb, 0, -1, false, vim.split(b, "\n"))
              vim.bo[ba].filetype = "json"
              vim.bo[bb].filetype = "json"
              vim.api.nvim_buf_set_name(ba, "response-latest")
              vim.api.nvim_buf_set_name(bb, string.format("response-%s", _response_history[idx].timestamp))

              vim.api.nvim_set_current_buf(ba)
              local win_a = vim.api.nvim_get_current_win()
              vim.cmd("vsplit")
              vim.api.nvim_set_current_buf(bb)
              local win_b = vim.api.nvim_get_current_win()
              vim.cmd("windo diffthis")

              local function clear_diff()
                for _, w in ipairs({ win_a, win_b }) do
                  if vim.api.nvim_win_is_valid(w) then
                    pcall(vim.api.nvim_win_call, w, function() vim.cmd("diffoff") end)
                  end
                end
              end

              local diff_aug = vim.api.nvim_create_augroup(
                "RestDiffCleanup_" .. vim.fn.sha256(tostring(ba)):sub(1, 8),
                { clear = true }
              )
              vim.api.nvim_create_autocmd("WinClosed", {
                group    = diff_aug,
                pattern  = { tostring(win_a), tostring(win_b) },
                once     = true,
                callback = function()
                  vim.schedule(clear_diff)
                  pcall(vim.api.nvim_del_augroup_by_id, diff_aug)
                end,
                desc = "Clear diffmode when REST diff windows close",
              })
            end
          )
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
        desc = "REST Insert auth token at cursor",
        ft   = "http",
      },
    },
  },

  shared.treesitter({ "http", "json" }),
}

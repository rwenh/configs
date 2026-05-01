-- lua/plugins/specs/lang/rest.lua — REST client (rest.nvim v3)
--

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
        vim.notify(
          "[rest] curl not found — rest.nvim will not function.\n"
          .. "Install curl: sudo zypper in curl",
          vim.log.levels.WARN
        )
        return {}
      end

      local env_file = ".env"
      local env_pattern = vim.pesc(env_file) .. "$"

      return {
        client       = "curl",
        env_file     = env_file,
        env_pattern  = env_pattern,
        env_edit_command = "tabedit",
        skip_ssl_verification   = false,
        custom_dynamic_variables = {},
        logs = {
          level = "info",
          save  = true,
        },
        result = {
          split = {
            horizontal                       = false,
            in_place                         = false,
            stay_at_current_window_after_split = true,
          },
          behavior = {
            decode_url  = true,
            show_info   = {
              url         = true,
              headers     = true,
              http_info   = true,
              curl_command = true,
            },
            statistics  = {
              enable = true,
              stats  = {
                { "total_time",      "Time taken:"    },
                { "size_download_t", "Download size:" },
              },
            },
            formatters = {
              json = vim.fn.executable("jq") == 1 and "jq" or nil,
              html = { cmd = { "prettier", "--parser", "html" } },
            },
          },
        },
        highlight = { enable = true, timeout = 750 },
        request   = {
          hooks = {
            encode_url       = true,
            user_agent       = "rest.nvim",
            set_content_type = true,
          },
        },
      }
    end)(),

    config = function(_, opts)
      if vim.tbl_isempty(opts) then return end   -- curl absent; skip setup
      pcall(function() require("rest-nvim").setup(opts) end)
    end,

    keys = {
      { "<leader>rer", "<cmd>Rest run<cr>",        desc = "REST Run Request",   ft = "http" },
      { "<leader>rel", "<cmd>Rest run last<cr>",   desc = "REST Run Last",      ft = "http" },
      { "<leader>rep", "<cmd>Rest preview<cr>",    desc = "REST Preview",       ft = "http" },
      { "<leader>ree", "<cmd>Rest env select<cr>", desc = "REST Select Env (.env / .env.dev / .env.prod)", ft = "http" },
    },
  },

  -- ── Treesitter ──────────────────────────────────────────────────────────────

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "http", "json" })
      end
    end,
  },
}

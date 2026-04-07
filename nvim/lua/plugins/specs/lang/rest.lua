-- lua/plugins/specs/lang/rest.lua - REST client (v3 API)
--
-- FIX (v2.2.5):
--   • version = "^2" removed. rest-nvim is now on v3+; the ^2 semver
--     constraint blocked installation entirely on systems with v3 in the
--     registry, and forced a downgrade on systems that already had v3.
--     version=false tracks the default branch (v3 stable).
--   • v2 API note retained: all calls are command-based (:Rest run etc.)
--     which is compatible with both v2 and v3.

return {
  {
    "rest-nvim/rest.nvim",
    ft           = "http",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
    opts = {
      client = "curl",
      env_file     = ".env",
      env_pattern  = "\\.env$",
      env_edit_command = "tabedit",
      skip_ssl_verification = false,
      custom_dynamic_variables = {},
      logs = {
        level  = "info",
        save   = true,
      },
      result = {
        split = {
          horizontal = false,
          in_place   = false,
          stay_at_current_window_after_split = true,
        },
        behavior = {
          decode_url        = true,
          show_info         = { url = true, headers = true, http_info = true, curl_command = true },
          statistics        = { enable = true, stats = {
            { "total_time", "Time taken:" },
            { "size_download_t", "Download size:" },
          }},
          formatters = {
            json = "jq",
            html = { cmd = { "prettier", "--parser", "html" } },
          },
        },
      },
      highlight = { enable = true, timeout = 750 },
      request = {
        hooks = {
          encode_url        = true,
          user_agent        = "rest.nvim",
          set_content_type  = true,
        },
      },
    },
    config = function(_, opts)
      pcall(function() require("rest-nvim").setup(opts) end)
    end,
    keys = {
      -- FIX: v2 API is command-based — :Rest run / run last / preview / env select
      {
        "<leader>rer",
        "<cmd>Rest run<cr>",
        desc = "REST Run Request",
        ft   = "http",
      },
      {
        "<leader>rel",
        "<cmd>Rest run last<cr>",
        desc = "REST Run Last",
        ft   = "http",
      },
      {
        "<leader>rep",
        "<cmd>Rest preview<cr>",
        desc = "REST Preview Request",
        ft   = "http",
      },
      {
        "<leader>ree",
        "<cmd>Rest env select<cr>",
        desc = "REST Select Env",
        ft   = "http",
      },
    },
  },

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

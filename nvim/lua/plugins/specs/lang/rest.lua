-- lua/plugins/specs/lang/rest.lua - REST client (v2 API)

return {
  {
    "rest-nvim/rest.nvim",
    ft           = "http",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
    version = "^2",
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
          statistics        = { enable = true, stats = { { "total_time", "Time taken:" }, { "size_download_t", "Download size:" } } },
          formatters        = { json = "jq", html = { cmd = { "prettier", "--parser", "html" } } },
        },
      },
      highlight = { enable = true, timeout = 750 },
      request = {
        hooks = {
          encode_url     = true,
          user_agent     = "rest.nvim",
          set_content_type = true,
        },
      },
    },
    config = function(_, opts)
      pcall(function() require("rest-nvim").setup(opts) end)
    end,
    keys = {
      {
        "<leader>rer",
        function() pcall(function() require("rest-nvim").run() end) end,
        desc = "REST Run Request",
        ft   = "http",
      },
      {
        "<leader>rel",
        function() pcall(function() require("rest-nvim").run_last() end) end,
        desc = "REST Run Last",
        ft   = "http",
      },
      {
        "<leader>rep",
        function() pcall(function() require("rest-nvim").preview() end) end,
        desc = "REST Preview Request",
        ft   = "http",
      },
      {
        "<leader>ree",
        function()
          local env = vim.fn.input("Env file: ", ".env", "file")
          if env ~= "" then
            pcall(function() require("rest-nvim").select_env(env) end)
          end
        end,
        desc = "REST Select Env",
        ft   = "http",
      },
    },
  },

  -- Treesitter parser for .http files
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

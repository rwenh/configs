-- lua/plugins/specs/lang/rest.lua - REST client (rest.nvim v2 API)
-- v2 dropped <Plug> mappings in favour of :Rest <subcommand> and lua functions.

return {
  {
    "rest-nvim/rest.nvim",
    ft           = "http",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
    -- Pin to v2 stable; v1 is abandoned and uses the old <Plug> API
    version = "^2",
    opts = {
      client = "curl",
      env_file     = ".env",
      env_pattern  = "\\.env$",
      env_edit_command = "tabedit",
      -- NOTE: encode_url belongs inside request.hooks, not here; removed duplicate top-level key
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
      -- FIX: removed stray ---@param annotation (was above a table key, not a function)
      request = {
        hooks = {
          encode_url     = true,
          user_agent     = "rest.nvim",
          set_content_type = true,
        },
      },
    },
    keys = {
      {
        -- FIX: renamed <leader>h* → <leader>re* to give Harpoon sole ownership of <leader>h*
        "<leader>rer",
        function() require("rest-nvim").run() end,
        desc = "REST Run Request",
        ft   = "http",
      },
      {
        "<leader>rel",
        -- FIX: .last() is the v1 API; v2 renamed it to .run_last()
        function() require("rest-nvim").run_last() end,
        desc = "REST Run Last",
        ft   = "http",
      },
      {
        "<leader>rep",
        function() require("rest-nvim").preview() end,
        desc = "REST Preview Request",
        ft   = "http",
      },
      {
        "<leader>ree",
        function()
          local env = vim.fn.input("Env file: ", ".env", "file")
          if env ~= "" then
            require("rest-nvim").select_env(env)
          end
        end,
        desc = "REST Select Env",
        ft   = "http",
      },
    },
  },

  -- Treesitter parser for .http files (required by rest.nvim v2)
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

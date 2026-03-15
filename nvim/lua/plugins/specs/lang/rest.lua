-- lua/plugins/specs/lang/rest.lua - REST client

return {
  {
    "rest-nvim/rest.nvim",
    ft           = "http",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      result = {
        show_url       = true,
        show_http_info = true,
        show_headers   = true,
        formatters = {
          json = "jq",
          html = { cmd = { "prettier", "--parser", "html" } },
        },
      },
      env_file     = ".env",
      env_pattern  = "\\.env$",
      skip_ssl_verification = false,
    },
    keys = {
      {
        "<leader>hr",
        "<Plug>RestNvim",
        desc = "REST Run Request",
        ft   = "http",
      },
      {
        "<leader>hp",
        "<Plug>RestNvimPreview",
        desc = "REST Preview Request",
        ft   = "http",
      },
      {
        "<leader>hl",
        "<Plug>RestNvimLast",
        desc = "REST Run Last",
        ft   = "http",
      },
      {
        "<leader>he",
        function()
          local env = vim.fn.input("Env file: ", ".env", "file")
          if env ~= "" then
            vim.cmd("RestSelectEnv " .. env)
          end
        end,
        desc = "REST Select Env",
        ft   = "http",
      },
    },
  },
}

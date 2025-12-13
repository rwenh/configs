-- ~/.config/nvim/lua/plugins/lsp.lua

return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    build = ":MasonUpdate",
    config = function()
      require("mason").setup({
        ui = {
          border = "rounded",
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗"
          }
        },
        max_concurrent_installers = 4,
      })
    end,
  },

  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = { "mason.nvim" },
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local ensure_installed = {
        "lua_ls", "pyright", "rust_analyzer", "fortls",
        "sqlls", "html", "cssls", "ts_ls",
        "jsonls", "yamlls", "marksman", "clangd"
      }

      -- RECALIBRATED: Only add gopls if Go 1.25+ is available
      local helpers = require("utils.helpers")
      if helpers.command_exists("go") then
        local go_version_output = vim.fn.system("go version")
        local major, minor = go_version_output:match("go(%d+)%.(%d+)")

        if major and minor then
          major, minor = tonumber(major), tonumber(minor)

          if (major > 1) or (major == 1 and minor >= 25) then
            table.insert(ensure_installed, "gopls")
          end
        end
      end

      require("mason-lspconfig").setup({
        ensure_installed = ensure_installed,
        automatic_installation = true,
      })
    end,
  },

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
      "b0o/schemastore.nvim",
    },
    event = { "BufReadPre", "BufNewFile" },
  },
}

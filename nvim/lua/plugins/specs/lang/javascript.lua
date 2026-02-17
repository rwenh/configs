-- lua/plugins/specs/lang/javascript.lua
-- autotag is centrally managed in web.lua

return {
  {
    "mxsdev/nvim-dap-vscode-js",
    ft           = "javascript",
    dependencies = { "mfussenegger/nvim-dap" },
    opts         = { adapters = { "pwa-node", "pwa-chrome" } },
  },
}

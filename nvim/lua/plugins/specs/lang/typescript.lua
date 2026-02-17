-- lua/plugins/specs/lang/typescript.lua
-- autotag is centrally managed in web.lua

return {
  {
    "mxsdev/nvim-dap-vscode-js",
    ft           = "typescript",
    dependencies = { "mfussenegger/nvim-dap" },
    opts         = { adapters = { "pwa-node", "pwa-chrome" } },
  },
}

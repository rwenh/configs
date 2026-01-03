-- lua/plugins/specs/lang/javascript. lua - JavaScript development

return {
  {
    "windwp/nvim-ts-autotag",
    ft = { "javascript", "jsx" },
    opts = {},
  },

  {
    "mxsdev/nvim-dap-vscode-js",
    ft = "javascript",
    dependencies = { "mfussenegger/nvim-dap" },
    opts = {
      adapters = { "pwa-node", "pwa-chrome" },
    },
  },
}
-- lua/plugins/specs/lang/typescript.lua - TypeScript development

return {
  {
    "windwp/nvim-ts-autotag",
    ft = { "typescript", "typescriptreact", "tsx" },
    opts = {},
  },

  {
    "mxsdev/nvim-dap-vscode-js",
    ft = "typescript",
    dependencies = { "mfussenegger/nvim-dap" },
    opts = {
      adapters = { "pwa-node", "pwa-chrome" },
    },
  },
}
-- lua/plugins/specs/lang/go.lua - Go development

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft = { "go", "gomod" },
    config = function()
      require("go"). setup()
    end,
  },
}

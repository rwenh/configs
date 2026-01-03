-- lua/plugins/specs/lang/java. lua - Java development

return {
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    dependencies = { "mfussenegger/nvim-dap", "williamboman/mason.nvim" },
    config = function()
      local jdtls = require("jdtls")
      local root_dir = require("jdtls.setup").find_root({ ". git", "mvnw", "gradlew", "pom.xml", "build.gradle" })
      jdtls.start_or_attach({
        root_dir = root_dir,
      })
    end,
  },
}

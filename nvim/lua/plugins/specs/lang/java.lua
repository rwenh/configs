-- lua/plugins/specs/lang/java.lua - Java development (SAFE KEYMAPS)

return {
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    dependencies = { "mfussenegger/nvim-dap", "williamboman/mason.nvim" },
    config = function()
      local jdtls = require("jdtls")
      local root_dir = require("jdtls.setup").find_root({ ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" })
      local workspace_dir = vim.fn.stdpath("data") .. "/jdtls-workspace/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")
      
      -- Path to java-debug
      local bundles = {
        vim.fn.glob(vim.fn.stdpath("data") .. "/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar", 1)
      }
      
      -- Path to java-test
      vim.list_extend(bundles, vim.split(vim.fn.glob(vim.fn.stdpath("data") .. "/mason/packages/java-test/extension/server/*.jar", 1), "\n"))
      
      local config = {
        cmd = {
          "java",
          "-Declipse.application=org.eclipse.jdt.ls.core.id1",
          "-Dosgi.bundles.defaultStartLevel=4",
          "-Declipse.product=org.eclipse.jdt.ls.core.product",
          "-Dlog.protocol=true",
          "-Dlog.level=ALL",
          "-Xms1g",
          "--add-modules=ALL-SYSTEM",
          "--add-opens", "java.base/java.util=ALL-UNNAMED",
          "--add-opens", "java.base/java.lang=ALL-UNNAMED",
          "-jar", vim.fn.glob(vim.fn.stdpath("data") .. "/mason/packages/jdtls/plugins/org.eclipse.equinox.launcher_*.jar"),
          "-configuration", vim.fn.stdpath("data") .. "/mason/packages/jdtls/config_linux",
          "-data", workspace_dir,
        },
        root_dir = root_dir,
        settings = {
          java = {
            signatureHelp = { enabled = true },
            completion = { enabled = true },
          },
        },
        init_options = {
          bundles = bundles,
        },
        on_attach = function(client, bufnr)
          -- Setup DAP
          jdtls.setup_dap({ hotcodereplace = "auto" })
          jdtls.setup.add_commands()
          
          -- Setup test support
          require("jdtls.dap").setup_dap_main_class_configs()
          
          -- Keymaps (using 'jv' for Java prefix - safe)
          local opts = { buffer = bufnr }
          vim.keymap.set("n", "<leader>jvo", "<cmd>lua require'jdtls'.organize_imports()<cr>", { buffer = bufnr, desc = "Java Organize Imports" })
          vim.keymap.set("n", "<leader>jvv", "<cmd>lua require'jdtls'.extract_variable()<cr>", { buffer = bufnr, desc = "Java Extract Variable" })
          vim.keymap.set("v", "<leader>jvv", "<esc><cmd>lua require'jdtls'.extract_variable(true)<cr>", { buffer = bufnr, desc = "Java Extract Variable" })
          vim.keymap.set("n", "<leader>jvc", "<cmd>lua require'jdtls'.extract_constant()<cr>", { buffer = bufnr, desc = "Java Extract Constant" })
          vim.keymap.set("v", "<leader>jvc", "<esc><cmd>lua require'jdtls'.extract_constant(true)<cr>", { buffer = bufnr, desc = "Java Extract Constant" })
          vim.keymap.set("v", "<leader>jvm", "<esc><cmd>lua require'jdtls'.extract_method(true)<cr>", { buffer = bufnr, desc = "Java Extract Method" })
          vim.keymap.set("n", "<leader>jvt", "<cmd>lua require'jdtls'.test_class()<cr>", { buffer = bufnr, desc = "Java Test Class" })
          vim.keymap.set("n", "<leader>jvn", "<cmd>lua require'jdtls'.test_nearest_method()<cr>", { buffer = bufnr, desc = "Java Test Nearest Method" })
        end,
      }
      
      jdtls.start_or_attach(config)
    end,
  },
}

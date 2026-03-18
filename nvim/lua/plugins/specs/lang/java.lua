-- lua/plugins/specs/lang/java.lua - Java development (cross-platform)

return {
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    dependencies = { "mfussenegger/nvim-dap", "williamboman/mason.nvim" },
    config = function()
      local jdtls     = require("jdtls")
      local root_dir  = require("jdtls.setup").find_root({ ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" })
      local data_dir  = vim.fn.stdpath("data")
      local workspace = data_dir .. "/jdtls-workspace/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")

      -- Cross-platform OS detection
      local os_map = { Linux = "linux", Darwin = "mac", Windows_NT = "win" }
      local os_key = os_map[vim.uv.os_uname().sysname] or "linux"
      local config_dir = data_dir .. "/mason/packages/jdtls/config_" .. os_key

      -- java-debug bundles
      local bundles = {
        vim.fn.glob(data_dir .. "/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar", 1),
      }

      -- java-test bundles
      vim.list_extend(bundles, vim.split(
        vim.fn.glob(data_dir .. "/mason/packages/java-test/extension/server/*.jar", 1), "\n"
      ))
      -- Remove empty strings from glob misses
      bundles = vim.tbl_filter(function(b) return b ~= "" end, bundles)

      local launcher = vim.fn.glob(data_dir .. "/mason/packages/jdtls/plugins/org.eclipse.equinox.launcher_*.jar")

      if launcher == "" then
        vim.notify("[java] jdtls launcher not found — run :MasonInstall jdtls", vim.log.levels.ERROR)
        return
      end

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
          "-jar",        launcher,
          "-configuration", config_dir,
          "-data",       workspace,
        },
        root_dir = root_dir,
        settings = {
          java = {
            signatureHelp = { enabled = true },
            completion    = { enabled = true },
            format        = { enabled = true },
          },
        },
        init_options = {
          bundles = bundles,
        },
        on_attach = function(_, bufnr)
          -- Setup DAP
          jdtls.setup_dap({ hotcodereplace = "auto" })
          jdtls.setup.add_commands()
          pcall(function() require("jdtls.dap").setup_dap_main_class_configs() end)

          -- Keymaps
          local function map(lhs, rhs, desc)
            vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
          end
          local function mapv(lhs, rhs, desc)
            vim.keymap.set("v", lhs, rhs, { buffer = bufnr, desc = desc })
          end

          map("<leader>jvo", function() jdtls.organize_imports() end,            "Java Organize Imports")
          map("<leader>jvv", function() jdtls.extract_variable() end,            "Java Extract Variable")
          map("<leader>jvc", function() jdtls.extract_constant() end,            "Java Extract Constant")
          map("<leader>jvt", function() jdtls.test_class() end,                  "Java Test Class")
          map("<leader>jvn", function() jdtls.test_nearest_method() end,         "Java Test Nearest Method")
          mapv("<leader>jvv", function() jdtls.extract_variable(true) end,       "Java Extract Variable")
          mapv("<leader>jvc", function() jdtls.extract_constant(true) end,       "Java Extract Constant")
          mapv("<leader>jvm", function() jdtls.extract_method(true) end,         "Java Extract Method")
        end,
      }

      jdtls.start_or_attach(config)
    end,
  },
}

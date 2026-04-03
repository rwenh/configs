-- lua/plugins/specs/lang/java.lua - Java development (cross-platform)

return {
  {
    "mfussenegger/nvim-jdtls",
    ft           = "java",
    dependencies = { "mfussenegger/nvim-dap", "williamboman/mason.nvim" },
    config = function()
      -- RECALIBRATION: Safe jdtls with comprehensive error handling
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "java",
        group    = vim.api.nvim_create_augroup("JdtlsAttach", { clear = true }),
        callback = function()
          local ok, jdtls = pcall(require, "jdtls")
          if not ok then
            vim.notify("nvim-jdtls failed to load", vim.log.levels.ERROR)
            return
          end

          local data_dir = vim.fn.stdpath("data")

          local ok_setup, setup = pcall(require, "jdtls.setup")
          if not ok_setup then
            vim.notify("jdtls.setup failed to load", vim.log.levels.ERROR)
            return
          end

          local root_dir = setup.find_root(
            { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" }
          )
          root_dir = root_dir or vim.fn.getcwd()

          local workspace = data_dir .. "/jdtls-workspace/"
            .. vim.fn.fnamemodify(root_dir, ":p:h:t")

          -- Cross-platform OS detection
          local os_map    = { Linux = "linux", Darwin = "mac", Windows_NT = "win" }
          local os_key    = os_map[vim.uv.os_uname().sysname] or "linux"
          local config_dir = data_dir .. "/mason/packages/jdtls/config_" .. os_key

          -- Safe glob for bundles
          local bundles = vim.split(
            vim.fn.glob(data_dir .. "/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar"),
            "\n", { plain = true, trimempty = true }
          )
          vim.list_extend(bundles, vim.split(
            vim.fn.glob(data_dir .. "/mason/packages/java-test/extension/server/*.jar"),
            "\n", { plain = true, trimempty = true }
          ))

          local launcher = vim.fn.glob(
            data_dir .. "/mason/packages/jdtls/plugins/org.eclipse.equinox.launcher_*.jar"
          )

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
              "-jar",           launcher,
              "-configuration", config_dir,
              "-data",          workspace,
            },
            root_dir = root_dir,
            settings = {
              java = {
                signatureHelp = { enabled = true },
                completion    = { enabled = true },
                format        = { enabled = true },
              },
            },
            init_options = { bundles = bundles },
            on_attach = function(_, bufnr)
              pcall(function() jdtls.setup_dap({ hotcodereplace = "auto" }) end)
              pcall(function() require("jdtls.dap").setup_dap_main_class_configs() end)

              local function map(lhs, rhs, desc)
                vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
              end
              local function mapv(lhs, rhs, desc)
                vim.keymap.set("v", lhs, rhs, { buffer = bufnr, desc = desc })
              end

              map("<leader>jvo",  function() pcall(function() jdtls.organize_imports() end) end,           "Java Organize Imports")
              map("<leader>jvv",  function() pcall(function() jdtls.extract_variable() end) end,           "Java Extract Variable")
              map("<leader>jvc",  function() pcall(function() jdtls.extract_constant() end) end,           "Java Extract Constant")
              map("<leader>jvt",  function() pcall(function() jdtls.test_class() end) end,                 "Java Test Class")
              map("<leader>jvn",  function() pcall(function() jdtls.test_nearest_method() end) end,        "Java Test Nearest Method")
              mapv("<leader>jvv", function() pcall(function() jdtls.extract_variable(true) end) end,       "Java Extract Variable")
              mapv("<leader>jvc", function() pcall(function() jdtls.extract_constant(true) end) end,       "Java Extract Constant")
              mapv("<leader>jvm", function() pcall(function() jdtls.extract_method(true) end) end,         "Java Extract Method")
            end,
          }

          pcall(function() jdtls.start_or_attach(config) end)
        end,
      })
    end,
  },
}

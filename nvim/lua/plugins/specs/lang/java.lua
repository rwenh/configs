-- lua/plugins/specs/lang/java.lua — Java development
--
-- LSP:    jdtls via nvim-jdtls (this file — not lspconfig/mason-lspconfig)
-- Format: jdtls built-in formatter (no conform entry needed)
-- DAP:    java-debug-adapter via jdtls on_attach + dap.lua configurations
-- Test:   neotest-java via test.lua; java-test via jdtls bundles
-- Docs:   neogen optional spec (this file)
--
-- DAP architecture:
--   jdtls.setup_dap() in on_attach registers the adapter.
--   dap.lua registers dap.configurations.java on FileType.
--   Both are needed; they are complementary not redundant.
--

return {
  {
    "mfussenegger/nvim-jdtls",
    ft           = "java",
    dependencies = { "mfussenegger/nvim-dap", "williamboman/mason.nvim" },
    config = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "java",
        group    = vim.api.nvim_create_augroup("JdtlsAttach", { clear = true }),
        callback = function(e)
          if vim.b[e.buf].jdtls_started then return end

          local ok_jdtls, jdtls = pcall(require, "jdtls")
          if not ok_jdtls then
            vim.notify("nvim-jdtls failed to load", vim.log.levels.ERROR)
            return
          end

          local ok_setup, setup = pcall(require, "jdtls.setup")
          if not ok_setup then
            vim.notify("jdtls.setup failed to load", vim.log.levels.ERROR)
            return
          end

          local data_dir  = vim.fn.stdpath("data")

          local mason_root = (function()
            local ok_mr, mr = pcall(require, "mason-registry")
            if ok_mr and mr.get_package then
              local ok_pkg, pkg = pcall(mr.get_package, "jdtls")
              if ok_pkg and pkg then return pkg:get_install_path() end
            end
            return data_dir .. "/mason/packages/jdtls"
          end)()

          local root_dir = setup.find_root(
            { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" }
          ) or vim.fn.getcwd()

          local buf_path  = vim.api.nvim_buf_get_name(e.buf)
          local hash_src  = (buf_path ~= "" and buf_path ~= root_dir)
            and buf_path or root_dir
          local workspace = data_dir .. "/jdtls-workspace/"
            .. vim.fn.sha256(hash_src):sub(1, 16)

          local config_dir = mason_root .. "/config_linux"

          local bundles = vim.split(
            vim.fn.glob(mason_root:gsub("jdtls$", "java-debug-adapter")
              .. "/extension/server/com.microsoft.java.debug.plugin-*.jar"),
            "\n", { plain = true, trimempty = true }
          )
          vim.list_extend(bundles, vim.split(
            vim.fn.glob(mason_root:gsub("jdtls$", "java-test")
              .. "/extension/server/*.jar"),
            "\n", { plain = true, trimempty = true }
          ))

          local launcher = vim.fn.glob(
            mason_root .. "/plugins/org.eclipse.equinox.launcher_*.jar"
          )
          if launcher == "" then
            vim.notify(
              "[java] jdtls launcher not found — run :MasonInstall jdtls",
              vim.log.levels.ERROR
            )
            return
          end

          local function build_jdtls_config()
            return {
              cmd = {
                "java",
                "-Declipse.application=org.eclipse.jdt.ls.core.id1",
                "-Dosgi.bundles.defaultStartLevel=4",
                "-Declipse.product=org.eclipse.jdt.ls.core.product",
                "-Dlog.protocol=true", "-Dlog.level=ALL", "-Xms1g",
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
                local bkm = require("core.util.buf_keymap")

                local _dap_ready = false
                local function ensure_dap()
                  if _dap_ready then return end
                  _dap_ready = true
                  pcall(function() jdtls.setup_dap({ hotcodereplace = "auto" }) end)
                  pcall(function() require("jdtls.dap").setup_dap_main_class_configs() end)
                end

                local has_java_test = #vim.tbl_filter(function(b)
                  return b:find("junit", 1, true) or b:find("java%-test", 1, true)
                end, bundles) > 0

                local java_maps = {
                  { "n", "<leader>jvo", function()
                      pcall(function() jdtls.organize_imports() end)
                    end, "Java Organize Imports" },
                  { "n", "<leader>jvv", function()
                      pcall(function() jdtls.extract_variable() end)
                    end, "Java Extract Variable" },
                  { "v", "<leader>jvv", function()
                      pcall(function() jdtls.extract_variable(true) end)
                    end, "Java Extract Variable (visual)" },
                  { "n", "<leader>jvc", function()
                      pcall(function() jdtls.extract_constant() end)
                    end, "Java Extract Constant" },
                  { "v", "<leader>jvc", function()
                      pcall(function() jdtls.extract_constant(true) end)
                    end, "Java Extract Constant (visual)" },
                  { "v", "<leader>jvm", function()
                      pcall(function() jdtls.extract_method(true) end)
                    end, "Java Extract Method" },
                }

                if has_java_test then
                  vim.list_extend(java_maps, {
                    { "n", "<leader>jvt", function()
                        ensure_dap()
                        pcall(function() jdtls.test_class() end)
                      end, "Java Test Class" },
                    { "n", "<leader>jvn", function()
                        ensure_dap()
                        pcall(function() jdtls.test_nearest_method() end)
                      end, "Java Test Nearest Method" },
                  })
                end

                bkm.batch(bufnr, java_maps)
              end,
            }
          end

          local start_ok = pcall(function()
            jdtls.start_or_attach(build_jdtls_config())
          end)
          if start_ok then
            vim.b[e.buf].jdtls_started = true
          else
            vim.notify("[java] jdtls failed to start — will retry on next open",
              vim.log.levels.WARN)
          end
        end,
      })
    end,
  },

  -- ── Neogen: javadoc ────────────────────────────────────────────────────────

  {
    "danymat/neogen",
    optional = true,
    ft       = "java",
    opts = {
      languages = {
        java = { template = { annotation_convention = "javadoc" } },
      },
    },
    keys = {
      {
        "<leader>jvg",
        function() pcall(function() require("neogen").generate() end) end,
        desc = "Java Generate Javadoc (alias for <leader>xg)",
        ft   = "java",
      },
    },
  },
}

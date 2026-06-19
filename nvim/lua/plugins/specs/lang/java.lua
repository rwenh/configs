-- lua/plugins/specs/lang/java.lua — Java development
--

-- ── Spring Boot detection ─────────────────────────────────────────────────
local function is_spring_project(root)
  root = root or vim.fn.getcwd()
  for _, fname in ipairs({ "build.gradle", "build.gradle.kts", "pom.xml" }) do
    local f = root .. "/" .. fname
    if vim.fn.filereadable(f) == 1 then
      local lines = vim.fn.readfile(f)
      for _, line in ipairs(lines) do
        if line:find("spring%-boot", 1, true) or line:find("springframework", 1, true) then
          return true
        end
      end
    end
  end
  return false
end

-- ── java-test bundle presence check ──────────────────────────────────────
--
--
local function detect_java_test(bundles, mason_root)
  -- Primary: the Mason java-test package directory exists and has JARs.
  local java_test_pkg = mason_root:gsub("jdtls$", "java-test")
  if vim.fn.isdirectory(java_test_pkg) == 1 then
    local jars = vim.fn.glob(java_test_pkg .. "/extension/server/*.jar")
    if jars ~= "" then return true end
  end

  for _, jar_path in ipairs(bundles) do
    local lower = jar_path:lower()
    if lower:find("java%-test", 1, true)
    or lower:find("junit", 1, true)
    or lower:find("testrunner", 1, true)
    or lower:find("com.microsoft.java.test", 1, true) then
      return true
    end
  end

  return false
end

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
          if vim.b[e.buf].jdtls_started then
            -- Verify a jdtls client is actually attached to this buffer.
            local clients = vim.lsp.get_clients({ bufnr = e.buf, name = "jdtls" })
            if #clients > 0 then return end
            -- No active client — clear the stale flag and re-attach.
            vim.b[e.buf].jdtls_started = nil
          end

          local ok_jdtls, jdtls = pcall(require, "jdtls")
          if not ok_jdtls then vim.notify("nvim-jdtls failed to load", vim.log.levels.ERROR); return end
          local ok_setup, setup = pcall(require, "jdtls.setup")
          if not ok_setup then vim.notify("jdtls.setup failed to load", vim.log.levels.ERROR); return end

          local data_dir    = vim.fn.stdpath("data")
          local mason_root  = (function()
            local ok_mr, mr = pcall(require, "mason-registry")
            if ok_mr and mr.get_package then
              local ok_pkg, pkg = pcall(mr.get_package, "jdtls")
              if ok_pkg and pkg then return pkg:get_install_path() end
            end
            return data_dir .. "/mason/packages/jdtls"
          end)()

          local root_dir = setup.find_root({ ".git","mvnw","gradlew","pom.xml","build.gradle" }) or vim.fn.getcwd()
          local buf_path = vim.api.nvim_buf_get_name(e.buf)
          local hash_src = (buf_path ~= "" and buf_path ~= root_dir) and buf_path or root_dir
          local workspace = data_dir .. "/jdtls-workspace/" .. vim.fn.sha256(hash_src)

          local config_dir = (function()
            local sysname = (vim.uv.os_uname() or {}).sysname or "Linux"
            if sysname:find("Windows") then return mason_root .. "/config_win"
            elseif sysname == "Darwin"  then return mason_root .. "/config_mac"
            else                             return mason_root .. "/config_linux"
            end
          end)()

          local function safe_glob_split(pattern, label)
            local result = vim.fn.glob(pattern)
            if result == "" then
              vim.schedule(function()
                vim.notify(string.format("[java] %s not found.\nRun: :MasonInstall %s", label, label:lower():gsub(" ", "-")), vim.log.levels.WARN)
              end)
              return {}
            end
            return vim.split(result, "\n", { plain = true, trimempty = true })
          end

          local bundles = safe_glob_split(mason_root:gsub("jdtls$", "java-debug-adapter") .. "/extension/server/com.microsoft.java.debug.plugin-*.jar", "java-debug-adapter")
          vim.list_extend(bundles, safe_glob_split(mason_root:gsub("jdtls$", "java-test") .. "/extension/server/*.jar", "java-test"))

          local launcher = vim.fn.glob(mason_root .. "/plugins/org.eclipse.equinox.launcher_*.jar")
          if launcher == "" then vim.notify("[java] jdtls launcher not found — run :MasonInstall jdtls", vim.log.levels.ERROR); return end

          local has_java_test = detect_java_test(bundles, mason_root)

          local function build_jdtls_config()
            return {
              cmd = { "java",
                "-Declipse.application=org.eclipse.jdt.ls.core.id1",
                "-Dosgi.bundles.defaultStartLevel=4",
                "-Declipse.product=org.eclipse.jdt.ls.core.product",
                "-Dlog.protocol=true", "-Dlog.level=ALL", "-Xms1g",
                "--add-modules=ALL-SYSTEM",
                "--add-opens", "java.base/java.util=ALL-UNNAMED",
                "--add-opens", "java.base/java.lang=ALL-UNNAMED",
                "-jar", launcher, "-configuration", config_dir, "-data", workspace,
              },
              root_dir     = root_dir,
              settings     = { java = { signatureHelp = { enabled = true }, completion = { enabled = true }, format = { enabled = true } } },
              init_options = { bundles = bundles },
              on_attach = function(client, bufnr)
                local bkm = require("core.util.buf_keymap")

                local _dap_ready = false
                local function ensure_dap()
                  if _dap_ready then return end
                  _dap_ready = true
                  pcall(function() jdtls.setup_dap({ hotcodereplace = "auto" }) end)
                  pcall(function() require("jdtls.dap").setup_dap_main_class_configs() end)
                end

                local java_maps = {
                  { "n", "<leader>jvo", function() pcall(function() jdtls.organize_imports() end) end, "Java Organize Imports" },
                  { "n", "<leader>jvv", function() pcall(function() jdtls.extract_variable() end) end, "Java Extract Variable" },
                  { "v", "<leader>jvv", function() pcall(function() jdtls.extract_variable(true) end) end, "Java Extract Variable (visual)" },
                  { "n", "<leader>jvc", function() pcall(function() jdtls.extract_constant() end) end, "Java Extract Constant" },
                  { "v", "<leader>jvc", function() pcall(function() jdtls.extract_constant(true) end) end, "Java Extract Constant (visual)" },
                  { "v", "<leader>jvm", function() pcall(function() jdtls.extract_method(true) end) end, "Java Extract Method" },
                  { "n", "<leader>jvR", function()
                      local ok_r, err = pcall(function() jdtls.update_project_config() end)
                      if ok_r then vim.notify("[java] Project config reloaded", vim.log.levels.INFO)
                      else vim.notify("[java] Reload failed: " .. tostring(err), vim.log.levels.WARN) end
                    end, "Java Hot-reload (update project config)" },
                }

                if has_java_test then
                  vim.list_extend(java_maps, {
                    { "n", "<leader>jvt", function() ensure_dap(); pcall(function() jdtls.test_class() end) end, "Java Test Class" },
                    { "n", "<leader>jvn", function() ensure_dap(); pcall(function() jdtls.test_nearest_method() end) end, "Java Test Nearest Method" },
                  })
                else
                  -- Inform the user why test keymaps are absent.
                  vim.schedule(function()
                    vim.notify(
                      "[java] java-test adapter not detected — <leader>jvt and <leader>jvn unavailable.\n"
                      .. "Run: :MasonInstall java-test  then restart Neovim.",
                      vim.log.levels.DEBUG
                    )
                  end)
                end

                if is_spring_project(root_dir) then
                  local ok_runner, runner = pcall(require, "core.util.runner")
                  if ok_runner then
                    vim.list_extend(java_maps, {
                      { "n", "<leader>jvs", function()
                          local cmd = runner.gradle_or_maven(root_dir, "bootRun")
                          if cmd then require("core.util.term").float(cmd, { close_on_exit = false })
                          else vim.notify("[java] bootRun requires Gradle or Maven", vim.log.levels.WARN) end
                        end, "Java Spring Boot Run" },
                      { "n", "<leader>jvS", function()
                          local cmd = runner.gradle_or_maven(root_dir, "bootJar")
                          if cmd then require("core.util.term").float(cmd)
                          else vim.notify("[java] bootJar requires Gradle or Maven", vim.log.levels.WARN) end
                        end, "Java Spring Boot Package (bootJar)" },
                    })
                  end
                end

                bkm.batch(bufnr, java_maps)

                vim.api.nvim_create_autocmd("LspDetach", {
                  buffer   = bufnr,
                  once     = true,
                  callback = function(ev)
                    if ev.data and ev.data.client_id == client.id then
                      vim.b[bufnr].jdtls_started = nil
                    end
                  end,
                  desc = "Clear jdtls_started flag on client detach",
                })
              end,
            }
          end

          local start_ok, start_err = pcall(function() jdtls.start_or_attach(build_jdtls_config()) end)
          if start_ok then
            vim.b[e.buf].jdtls_started = true
          else
            vim.notify("[java] jdtls failed to start: " .. tostring(start_err) .. "\nDelete ~/.local/share/nvim/jdtls-workspace/ and reopen.", vim.log.levels.WARN)
          end
        end,
      })
    end,
  },

  -- ── Neogen: javadoc ────────────────────────────────────────────────────────
  { "danymat/neogen", optional = true, ft = "java",
    opts = { languages = { java = { template = { annotation_convention = "javadoc" } } } },
    keys = {
      { "<leader>jvg", function() pcall(function() require("neogen").generate() end) end,
        desc = "Java Generate Javadoc", ft = "java" },
    } },
}

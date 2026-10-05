-- lua/plugins/specs/lang/rust.lua — Rust language support
--

local function toggle_cargo_features()
  local ok_path, path = pcall(require, "core.util.path")
  local root  = (ok_path and path.find_root()) or vim.fn.getcwd()
  local cargo = root .. "/Cargo.toml"
  if vim.fn.filereadable(cargo) ~= 1 then
    vim.notify("[rust] Cargo.toml not found", vim.log.levels.WARN); return
  end
  local ok_r, lines = pcall(vim.fn.readfile, cargo)
  if not ok_r then return end

  local features   = {}
  local in_section = false
  for _, line in ipairs(lines) do
    if     line:match("^%[features%]")       then in_section = true
    elseif in_section and line:match("^%[") then in_section = false
    elseif in_section then
      local name = line:match("^(%w[%w_%-]*)%s*=")
      if name and name ~= "default" then table.insert(features, name) end
    end
  end

  if #features == 0 then
    vim.notify("[rust] No non-default features found in Cargo.toml", vim.log.levels.INFO); return
  end

  local current = type(vim.g.rustaceanvim_features) == "table" and vim.g.rustaceanvim_features or {}
  local enabled = {}
  for _, f in ipairs(current) do enabled[f] = true end

  local items = vim.tbl_map(
    function(f) return (enabled[f] and "✓ " or "  ") .. f end,
    features
  )

  vim.ui.select(items, {
    prompt = "Toggle one Cargo feature (✓ = currently enabled) — repeat <leader>rf to toggle more:",
  }, function(choice, idx)
    if not choice or not idx then return end
    local feat = features[idx]
    if enabled[feat] then enabled[feat] = nil else enabled[feat] = true end
    local new_list = {}
    for f, _ in pairs(enabled) do table.insert(new_list, f) end
    table.sort(new_list)
    vim.g.rustaceanvim_features = new_list

    -- rustaceanvim re-evaluates `server.settings`
    pcall(vim.cmd, "RustAnalyzer reloadSettings")

    local active_str = #new_list > 0 and table.concat(new_list, ", ") or "(none)"
    vim.notify(
      "[rust] Active features: " .. active_str
      .. "\nPress <leader>rf again to toggle another feature.",
      vim.log.levels.INFO
    )
  end)
end

return {
  {
    "mrcjkb/rustaceanvim",
    version = "^5",
    lazy    = false,
    ft      = { "rust" },
    init = function()
      vim.g.rustaceanvim = {
        server = {
          on_attach = function(client, bufnr)
            if client.server_capabilities.inlayHintProvider then
              local ok, enabled = pcall(vim.lsp.inlay_hint.is_enabled, { bufnr = bufnr })
              if not (ok and enabled) then
                pcall(vim.lsp.inlay_hint.enable, true, { bufnr = bufnr })
              end
            end
          end,
          -- Dynamic so <leader>rf can change cargo features without a restart.
          settings = function(project_root, default_settings)
            local settings = require("rustaceanvim.config.server").load_rust_analyzer_settings(
              project_root, { default_settings = default_settings })
            local feats = vim.g.rustaceanvim_features
            if type(feats) == "table" then
              local ra = settings["rust-analyzer"]
              ra.cargo = vim.tbl_extend("force", ra.cargo or {}, { features = feats })
            end
            return settings
          end,
          default_settings = {
            ["rust-analyzer"] = {
              -- renamed keys: assist.importMergeBehavior/importPrefix -> imports.*;
              imports     = { granularity = { group = "module" }, prefix = "self" },
              cargo       = { features = "all" },
              procMacro   = { enable = true },
              check       = { command = "clippy", extraArgs = { "--no-deps" } },
              checkOnSave = true,
              inlayHints  = {
                parameterHints    = { enable = true },
                typeHints         = { enable = true },
                closingBraceHints = { minLines = 25 },
              },
            },
          },
        },
        -- No `dap.adapter` string: rustaceanvim auto-detects codelldb from Mason.
tools = { hover_actions = { auto_focus = false } },
      }
    end,

    keys = (function()
      local entries = {
        { "<leader>rh", "RustLsp hover",      "Rust Hover Actions" },
        { "<leader>ra", "RustLsp codeAction", "Rust Code Action"   },
        { "<leader>rd", "RustLsp debuggables","Rust Debuggables"   },
        { "<leader>rt", "RustLsp testables",  "Rust Testables"     },
      }
      local keys = {}
      for _, e in ipairs(entries) do
        table.insert(keys, { e[1], "<cmd>" .. e[2] .. "<cr>", desc = e[3], ft = "rust" })
      end
      table.insert(keys, {
        "<leader>rf",
        function() toggle_cargo_features() end,
        desc = "Rust Toggle one Cargo feature flag (repeat to toggle more)",
        ft   = "rust",
      })
      table.insert(keys, {
        "<leader>rx",
        function()
          if vim.fn.executable("cargo-expand") ~= 1 then
            vim.notify("[rust] cargo-expand not found.\nInstall: cargo install cargo-expand", vim.log.levels.WARN)
            return
          end
          local word    = vim.fn.expand("<cword>")
          local ok_path, path = pcall(require, "core.util.path")
          local root    = (ok_path and path.find_root()) or vim.fn.getcwd()
          local cmd     = "cd " .. vim.fn.shellescape(root) .. " && cargo expand"
          if word and word ~= "" then cmd = cmd .. " " .. vim.fn.shellescape(word) end
          local ok_term, term = pcall(require, "core.util.term")
          if ok_term then term.float(cmd) else vim.cmd("split | terminal " .. cmd) end
        end,
        desc = "Rust cargo expand (macro viewer)",
        ft   = "rust",
      })
      return keys
    end)(),
  },

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters = opts.formatters or {}
      local _edition_cache = {}
      local function detect_edition()
        local dir = vim.fn.expand("%:p:h")
        if dir == "" then dir = vim.fn.getcwd() end
        if _edition_cache[dir] then return _edition_cache[dir] end
        local found = "2021"
        for _, cargo in ipairs(vim.fs.find("Cargo.toml", { upward = true, path = dir, limit = math.huge })) do
          local ok, lines = pcall(vim.fn.readfile, cargo)
          if ok then
            local hit
            for _, line in ipairs(lines) do
              hit = line:match('^edition%s*=%s*"(%d+)"')
              if hit then break end
            end
            if hit then found = hit; break end
          end
        end
        _edition_cache[dir] = found
        return found
      end
      opts.formatters.rustfmt = {
        command = "rustfmt",
        args    = function() return { "--edition", detect_edition(), "--emit=stdout" } end,
        stdin   = true,
      }
    end,
  },
}

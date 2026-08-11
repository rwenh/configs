-- lua/plugins/specs/lang/web.lua — shared web tooling
--

local shared = require("plugins.specs.lang.shared")

vim.g.user_emmet_leader_key = "<C-e>"

-- ── PostCSS filetype registration ─────────────────────────────────────────
--

pcall(function()
  vim.filetype.add({
    extension = {
      pcss    = "css",
      postcss = "css",
    },
    filename = {
      ["postcss.config.js"]  = "javascript",
      ["postcss.config.cjs"] = "javascript",
      ["postcss.config.mjs"] = "javascript",
      ["postcss.config.ts"]  = "typescript",
    },
  })
end)

-- ── Vite dev-server detection ─────────────────────────────────────────────
--

local function is_vite_project()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  if not root or root == "" then return false end

  local configs = {
    "vite.config.ts", "vite.config.js",
    "vite.config.mts", "vite.config.mjs", "vite.config.cjs", "vite.config.cts",
  }
  for _, name in ipairs(configs) do
    if vim.fn.filereadable(root .. "/" .. name) == 1 then return true end
  end
  return false
end

-- ── Package-manager run-command mapping ────────────────────────────────────
--
local PM_RUN_PREFIX = {
  npm  = "npm run ",
  pnpm = "pnpm ",
  yarn = "yarn ",
  bun  = "bun run ",
}

---@param root   string
---@param script string  e.g. "dev" or "build"
---@return string
local function pm_run_cmd(root, script)
  local ok_runner, runner = pcall(require, "core.util.runner")
  local pm = (ok_runner and runner.detect_pkg_manager(root)) or "npm"
  return (PM_RUN_PREFIX[pm] or PM_RUN_PREFIX.npm) .. script
end

return {
  -- ── Auto-close HTML/JSX tags ───────────────────────────────────────────────
  {
    "windwp/nvim-ts-autotag",
    ft = shared.WEB_FT,
    opts = {
      opts = {
        enable_close          = true,
        enable_rename         = true,
        enable_close_on_slash = false,
      },
      per_filetype = {},
    },
    config = function(_, opts)
      local ok, err = pcall(function() require("nvim-ts-autotag").setup(opts) end)
      if not ok then
        vim.notify(
          "[web] nvim-ts-autotag setup failed: " .. tostring(err)
          .. "\nRun :Lazy update nvim-ts-autotag",
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── Emmet ──────────────────────────────────────────────────────────────────

  {
    "mattn/emmet-vim",
    ft = shared.WEB_FT,
  },

  -- ── Vite dev-server keymap ─────────────────────────────────────────────────
  --
  -- <leader>wv  — start the Vite dev server in a floating terminal.

  {
    "akinsho/toggleterm.nvim",
    ft = shared.JS_TS_FT,
    keys = {
      {
        "<leader>wv",
        function()
          if not is_vite_project() then
            vim.notify(
              "[web] No Vite config found in project root.\n"
              .. "Expected: vite.config.{ts,js,mts,mjs,cjs,cts}",
              vim.log.levels.WARN
            )
            return
          end

          local ok_path, path_util = pcall(require, "core.util.path")
          local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()

          require("core.util.term").float(
            "cd " .. vim.fn.shellescape(root) .. " && " .. pm_run_cmd(root, "dev"),
            { close_on_exit = false }
          )
        end,
        desc = "Web: Vite dev server",
        ft   = shared.JS_TS_FT,
      },

      {
        "<leader>wb",
        function()
          local ok_path, path_util = pcall(require, "core.util.path")
          local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()

          require("core.util.term").float(
            "cd " .. vim.fn.shellescape(root) .. " && " .. pm_run_cmd(root, "build")
          )
        end,
        desc = "Web: build (npm/pnpm/yarn/bun run build)",
        ft   = shared.JS_TS_FT,
      },
    },
  },
}

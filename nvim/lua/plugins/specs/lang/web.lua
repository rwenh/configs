-- lua/plugins/specs/lang/web.lua — shared web tooling: emmet, tag autoclose, Vite, postcss filetypes
--

local shared = require("plugins.specs.lang.shared")

-- ── Emmet ─────────────────────────────────────────────────────────────────────
-- Insert mode only. With the default install mode, emmet also maps <C-e> in normal and visual mode,
-- which takes scroll-down away from every web buffer. Expansion stays on <C-e> in insert mode.
vim.g.user_emmet_leader_key     = "<C-y>"
vim.g.user_emmet_expandabbr_key = "<C-e>"
vim.g.user_emmet_install_global = 0
vim.g.user_emmet_mode           = "i"

-- ── PostCSS filetypes ─────────────────────────────────────────────────────────
pcall(function()
  vim.filetype.add({
    extension = { pcss = "css", postcss = "css" },
    filename  = {
      ["postcss.config.js"]  = "javascript",
      ["postcss.config.cjs"] = "javascript",
      ["postcss.config.mjs"] = "javascript",
      ["postcss.config.ts"]  = "typescript",
    },
  })
end)

-- ── Vite and package-manager scripts ──────────────────────────────────────────
local VITE_CONFIGS = {
  "vite.config.ts", "vite.config.js", "vite.config.mts",
  "vite.config.mjs", "vite.config.cjs", "vite.config.cts",
}

local PM_RUN_PREFIX = {
  npm  = "npm run ",
  pnpm = "pnpm ",
  yarn = "yarn ",
  bun  = "bun run ",
}

local function is_vite_project(root)
  for _, name in ipairs(VITE_CONFIGS) do
    if vim.fn.filereadable(root .. "/" .. name) == 1 then return true end
  end
  return false
end

---@param script string
---@param opts   table?  forwarded to core.util.term.float
local function run_script(script, opts)
  local root = shared.project_root()
  local ok_runner, runner = pcall(require, "core.util.runner")
  local pm = (ok_runner and runner.detect_pkg_manager(root)) or "npm"
  if vim.fn.executable(pm) ~= 1 then
    vim.notify("[web] " .. pm .. " not found on PATH — cannot run '" .. script .. "'", vim.log.levels.ERROR)
    return
  end
  require("core.util.term").float(
    "cd " .. vim.fn.shellescape(root) .. " && " .. PM_RUN_PREFIX[pm] .. script,
    opts
  )
end

local function install_emmet(buf)
  if vim.fn.exists(":EmmetInstall") ~= 2 then return end
  vim.api.nvim_buf_call(buf, function()
    local ok, err = pcall(vim.cmd, "EmmetInstall")
    if not ok then
      vim.notify("[web] EmmetInstall failed: " .. tostring(err), vim.log.levels.WARN)
    end
  end)
end

return {
  -- ── Auto-close and auto-rename HTML/JSX tags ───────────────────────────────
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
        vim.notify("[web] nvim-ts-autotag setup failed: " .. tostring(err), vim.log.levels.WARN)
      end
    end,
  },

  -- ── Emmet ──────────────────────────────────────────────────────────────────
  {
    "mattn/emmet-vim",
    ft = shared.WEB_FT,
    config = function()
      -- The buffer that triggered lazy-loading missed the FileType event, so install for it now.
      install_emmet(vim.api.nvim_get_current_buf())
      vim.api.nvim_create_autocmd("FileType", {
        group    = vim.api.nvim_create_augroup("EmmetInstallWeb", { clear = true }),
        pattern  = shared.WEB_FT,
        callback = function(e) install_emmet(e.buf) end,
        desc     = "Install buffer-local emmet insert mappings in web buffers",
      })
    end,
  },

  -- ── Vite and build scripts ─────────────────────────────────────────────────
  {
    "akinsho/toggleterm.nvim",
    ft = shared.WEB_FT,
    keys = {
      {
        "<leader>wv",
        function()
          if not is_vite_project(shared.project_root()) then
            vim.notify(
              "[web] No Vite config found in project root.\n"
              .. "Expected one of: vite.config.{ts,js,mts,mjs,cjs,cts}",
              vim.log.levels.WARN
            )
            return
          end
          run_script("dev", { close_on_exit = false })
        end,
        desc = "Web: Vite dev server",
        ft   = shared.WEB_FT,
      },
      {
        "<leader>wb",
        function() run_script("build") end,
        desc = "Web: build (package manager run build)",
        ft   = shared.WEB_FT,
      },
    },
  },
}

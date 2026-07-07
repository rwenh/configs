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

          -- Prefer the local vite binary via npx/bunx/pnpx depending on the
          local ok_runner, runner = pcall(require, "core.util.runner")
          local ok_path,   path_util = pcall(require, "core.util.path")
          local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()

          local pm_cmd = "npm run dev"
          if ok_runner then
            local base = runner.detect_js_test_cmd(root)
            if base:find("^bun")  then pm_cmd = "bun run dev"
            elseif base:find("^pnpm") then pm_cmd = "pnpm dev"
            elseif base:find("^yarn") then pm_cmd = "yarn dev"
            end
          end

          require("core.util.term").float(
            "cd " .. vim.fn.shellescape(root) .. " && " .. pm_cmd,
            { close_on_exit = false }
          )
        end,
        desc = "Web: Vite dev server",
        ft   = shared.JS_TS_FT,
      },

      {
        "<leader>wb",
        function()
          local ok_runner, runner = pcall(require, "core.util.runner")
          local ok_path,   path_util = pcall(require, "core.util.path")
          local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()

          local pm_cmd = "npm run build"
          if ok_runner then
            local base = runner.detect_js_test_cmd(root)
            if base:find("^bun")  then pm_cmd = "bun run build"
            elseif base:find("^pnpm") then pm_cmd = "pnpm build"
            elseif base:find("^yarn") then pm_cmd = "yarn build"
            end
          end

          require("core.util.term").float(
            "cd " .. vim.fn.shellescape(root) .. " && " .. pm_cmd
          )
        end,
        desc = "Web: build (npm/pnpm/yarn/bun run build)",
        ft   = shared.JS_TS_FT,
      },
    },
  },
}

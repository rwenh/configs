-- lua/plugins/specs/lang/typescript.lua — TypeScript development
--

local shared = require("plugins.specs.lang.shared")

return {
  {
    "pmizio/typescript-tools.nvim",
    ft           = shared.JS_TS_FT,
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    opts = {
      settings = {
        tsserver_file_preferences = {
          importModuleSpecifierPreference         = vim.g.ts_import_preference or "non-relative",
          includeInlayParameterNameHints          = "literals",
          includeInlayVariableTypeHints           = false,
          includeInlayFunctionLikeReturnTypeHints = true,
        },
      },
    },
    config = function(_, opts)
      pcall(function() require("typescript-tools").setup(opts) end)
    end,
    keys = {
      { "<leader>tso", "<cmd>TSToolsOrganizeImports<cr>",          desc = "TS Organize Imports",         ft = shared.JS_TS_FT },
      { "<leader>tsi", "<cmd>TSToolsAddMissingImports<cr>",        desc = "TS Add Missing Imports",      ft = shared.JS_TS_FT },
      { "<leader>tsr", "<cmd>TSToolsRemoveUnusedImports<cr>",      desc = "TS Remove Unused Imports",    ft = shared.JS_TS_FT },
      { "<leader>tsf", "<cmd>TSToolsFixAll<cr>",                   desc = "TS Fix All Auto-fixable",     ft = shared.JS_TS_FT },
      { "<leader>tsd", "<cmd>TSToolsGoToSourceDefinition<cr>",     desc = "TS Go To Source Def",         ft = shared.JS_TS_FT },

      -- ── tsconfig.json jump ───────────────────────────────────────────────
      {
        "<leader>tsc",
        function()
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local candidates = {
            root .. "/tsconfig.json",
            root .. "/tsconfig.base.json",
            root .. "/tsconfig.app.json",
            root .. "/tsconfig.node.json",
          }
          for _, f in ipairs(candidates) do
            if vim.fn.filereadable(f) == 1 then
              vim.cmd("edit " .. vim.fn.fnameescape(f)); return
            end
          end
          vim.notify("[ts] No tsconfig.json found in project root", vim.log.levels.WARN)
        end,
        desc = "TS Jump to tsconfig.json",
        ft   = shared.JS_TS_FT,
      },

      -- ── Barrel file generator ────────────────────────────────────────────
      {
        "<leader>tsb",
        function()
          local dir   = vim.fn.expand("%:p:h")
          local files = vim.fn.globpath(dir, "*.ts",  false, true)
          local tsx   = vim.fn.globpath(dir, "*.tsx", false, true)
          vim.list_extend(files, tsx)

          local exports = {}
          for _, f in ipairs(files) do
            local base = vim.fn.fnamemodify(f, ":t:r")
            if base ~= "index" then
              table.insert(exports, 'export * from "./' .. base .. '";')
            end
          end

          if #exports == 0 then
            vim.notify("[ts] No exportable files found in " .. dir, vim.log.levels.WARN)
            return
          end

          table.sort(exports)
          local barrel = dir .. "/index.ts"

          local function do_write()
            local ok = pcall(vim.fn.writefile, exports, barrel)
            if ok then
              vim.notify("[ts] Barrel written: " .. barrel, vim.log.levels.INFO)
              vim.cmd("edit " .. vim.fn.fnameescape(barrel))
            else
              vim.notify("[ts] Failed to write barrel file", vim.log.levels.ERROR)
            end
          end

          if vim.fn.filereadable(barrel) == 1 then
            vim.ui.input(
              { prompt = "index.ts already exists — overwrite? [y/N]: " },
              function(ans)
                if ans == "y" or ans == "Y" then
                  do_write()
                else
                  vim.notify("[ts] Barrel generation cancelled.", vim.log.levels.INFO)
                end
              end
            )
          else
            do_write()
          end
        end,
        desc = "TS Generate barrel (index.ts) — prompts before overwrite",
        ft   = shared.JS_TS_FT,
      },

      -- ── Paths alias info ─────────────────────────────────────────────────
      {
        "<leader>tsp",
        function()
          local ok_path, path = pcall(require, "core.util.path")
          local root = (ok_path and path.find_root()) or vim.fn.getcwd()
          local tsconfig = vim.fn.findfile("tsconfig.json", root .. ";")
          if tsconfig == "" then
            vim.notify("[ts] tsconfig.json not found", vim.log.levels.WARN); return
          end
          local ok_r, lines = pcall(vim.fn.readfile, tsconfig)
          if not ok_r then return end
          local ok_j, cfg = pcall(vim.json.decode, table.concat(lines, "\n"))
          if not ok_j or type(cfg) ~= "table" then
            vim.notify("[ts] Could not parse tsconfig.json", vim.log.levels.WARN); return
          end
          local paths = cfg.compilerOptions and cfg.compilerOptions.paths
          if not paths or vim.tbl_isempty(paths) then
            vim.notify("[ts] No paths aliases defined in tsconfig.json", vim.log.levels.INFO)
            return
          end
          local lines_out = { "tsconfig paths aliases:" }
          for alias, targets in pairs(paths) do
            local t = type(targets) == "table" and targets[1] or tostring(targets)
            table.insert(lines_out, string.format("  %-30s → %s", alias, t))
          end
          vim.notify(table.concat(lines_out, "\n"), vim.log.levels.INFO)
        end,
        desc = "TS Show tsconfig paths aliases",
        ft   = shared.JS_TS_FT,
      },
    },
  },

  { "danymat/neogen", optional = true, ft = { "typescript","typescriptreact" },
    opts = { languages = { typescript = { template = { annotation_convention = "tsdoc" } } } } },

  shared.treesitter({ "typescript", "tsx" }),
}

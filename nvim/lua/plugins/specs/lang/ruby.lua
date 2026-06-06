-- lua/plugins/specs/lang/ruby.lua — Ruby language support
--
-- Test mechanism guide:
--   <leader>rbn/f/s  → vim-test (buffer-local, toggleterm strategy)
--   <leader>'n       → neotest-rspec (nearest test with rich output)
--   <leader>'t       → runner.lua (bundle exec rspec from project root)
--

local shared = require("plugins.specs.lang.shared")

-- ── Bundler version guard ─────────────────────────────────────────────────
--

local _bundler_checked = false

local function check_bundler_env()
  if _bundler_checked then return end
  _bundler_checked = true

  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()

  local lockfile = root .. "/Gemfile.lock"
  if vim.fn.filereadable(lockfile) ~= 1 then return end

  if vim.fn.executable("bundle") ~= 1 then
    vim.notify(
      "[ruby] `bundle` not found — install with: gem install bundler\n"
      .. "Without bundler, rspec and rubocop may use wrong gem versions.",
      vim.log.levels.WARN
    )
    return
  end

  if vim.fn.executable("rdbg") ~= 1 then
    local has_rdbg_in_bundle = vim.fn.system(
      "cd " .. vim.fn.shellescape(root)
      .. " && bundle list 2>/dev/null | grep debug"
    )
    if not has_rdbg_in_bundle:find("debug") then
      vim.notify(
        "[ruby] rdbg not found and not in bundle — DAP unavailable.\n"
        .. "Add to Gemfile: gem 'debug' OR run: gem install rdbg",
        vim.log.levels.WARN
      )
    end
  end
end

-- ── Rails detection ───────────────────────────────────────────────────────

local function is_rails_project()
  local ok_path, path_util = pcall(require, "core.util.path")
  local root = (ok_path and path_util.find_root()) or vim.fn.getcwd()
  return vim.fn.filereadable(root .. "/config/application.rb") == 1
end

return {
  -- ── vim-test ───────────────────────────────────────────────────────────────

  {
    "vim-test/vim-test",
    ft   = "ruby",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "ruby",
        once     = true,
        group    = vim.api.nvim_create_augroup("VimTestRubyInit", { clear = true }),
        callback = function()
          vim.api.nvim_create_autocmd("VimEnter", {
            pattern  = "*",
            once     = true,
            group    = vim.api.nvim_create_augroup("VimTestRubyStrategy", { clear = true }),
            callback = function()
              vim.g["test#strategy"]                  = "toggleterm"
              vim.g["test#toggleterm#reuse_terminal"] = 1
            end,
          })
          vim.schedule(check_bundler_env)
        end,
      })
    end,
    keys = (function()
      local function rkey(lhs, cmd, desc)
        return { lhs, "<cmd>" .. cmd .. "<cr>", desc = desc, ft = "ruby" }
      end
      return {
        rkey("<leader>rbn", "TestNearest", "Ruby Test Nearest"),
        rkey("<leader>rbf", "TestFile",    "Ruby Test File"   ),
        rkey("<leader>rbs", "TestSuite",   "Ruby Test Suite"  ),
        rkey("<leader>rbl", "TestLast",    "Ruby Test Last"   ),
        rkey("<leader>rbv", "TestVisit",   "Ruby Test Visit"  ),
      }
    end)(),
  },

  -- ── vim-rails ──────────────────────────────────────────────────────────────

  {
    "tpope/vim-rails",
    ft   = "ruby",
    cond = function() return is_rails_project() end,
  },

  -- ── Rails generator keymaps ────────────────────────────────────────────────
  --
  -- Only registered when a Rails project is detected (config/application.rb).
  -- Keymap layout:
  --   <leader>rgm  generate model
  --   <leader>rgc  generate controller
  --   <leader>rgs  generate scaffold
  --   <leader>rgj  generate job
  --   <leader>rgM  generate migration
  --   <leader>rgS  rails server
  --   <leader>rgd  db:migrate
  --   <leader>rgr  rails console

  {
    "akinsho/toggleterm.nvim",
    ft   = "ruby",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "ruby",
        once     = true,
        group    = vim.api.nvim_create_augroup("RailsGeneratorKeymaps", { clear = true }),
        callback = function()
          if not is_rails_project() then return end

          local bkm = require("core.util.buf_keymap")

          local function rails_generate(generator_type)
            vim.ui.input(
              { prompt = "rails generate " .. generator_type .. ": " },
              function(input)
                if not input or vim.trim(input) == "" then return end
                require("core.util.term").float_at_root(
                  "bundle exec rails generate " .. generator_type .. " " .. input
                )
              end
            )
          end

          local maps = {
            { "n", "<leader>rgm", function() rails_generate("model")      end, "Rails generate model"      },
            { "n", "<leader>rgc", function() rails_generate("controller") end, "Rails generate controller" },
            { "n", "<leader>rgs", function() rails_generate("scaffold")   end, "Rails generate scaffold"   },
            { "n", "<leader>rgj", function() rails_generate("job")        end, "Rails generate job"        },
            { "n", "<leader>rgM", function() rails_generate("migration")  end, "Rails generate migration"  },
            {
              "n", "<leader>rgS",
              function() require("core.util.term").float_at_root("bundle exec rails server") end,
              "Rails server",
            },
            {
              "n", "<leader>rgd",
              function() require("core.util.term").float_at_root("bundle exec rails db:migrate") end,
              "Rails db:migrate",
            },
            {
              "n", "<leader>rgr",
              function() require("core.util.term").float_at_root("bundle exec rails console") end,
              "Rails console",
            },
          }

          -- Apply to all currently open Ruby buffers.
          for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            if vim.api.nvim_buf_is_loaded(buf)
            and vim.bo[buf].filetype == "ruby" then
              bkm.batch(buf, maps, "ruby_rails_keymaps")
            end
          end

          -- And future ones.
          vim.api.nvim_create_autocmd("FileType", {
            pattern  = "ruby",
            group    = vim.api.nvim_create_augroup("RailsGeneratorFuture", { clear = true }),
            callback = function(e)
              if is_rails_project() then
                bkm.batch(e.buf, maps, "ruby_rails_keymaps")
              end
            end,
          })
        end,
      })
    end,
  },

  -- ── endwise ────────────────────────────────────────────────────────────────

  {
    "RRethy/nvim-treesitter-endwise",
    event        = "InsertEnter",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  shared.treesitter({ "ruby" }),
}

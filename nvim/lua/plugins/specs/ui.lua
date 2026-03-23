-- lua/plugins/specs/ui.lua - UI plugins
-- Only the ACTIVE theme loads eagerly (lazy=false). All others are lazy.

-- FIX #1: Defer theme lookup into a function rather than capturing it at
-- module load time. The top-level require("core.theme") ran during Lazy's
-- spec collection (before theme.setup()), meaning `active` was captured once
-- and never updated — switching themes via M.switch() would leave the wrong
-- plugin marked lazy=false for the session.
local function get_active() return require("core.theme").config.theme end

-- FIX #2: Tightened the fallback regex to handle plugin names with dots
-- (e.g. "author/some.plugin.nvim") by matching up to the first dot after
-- the last slash, rather than greedily stopping at any dot.
local function theme_spec(plugin_name, name, extra)
  local derived = name or (plugin_name:match("/([^/]+)%.nvim$") or plugin_name:match("/([^/]+)$"))
  return vim.tbl_extend("force", {
    plugin_name,
    name     = name or nil,
    lazy     = get_active() ~= derived,
    priority = 1000,
  }, extra or {})
end

return {

  -- ┌─────────────────────────────────────────────────────┐
  -- │                    THEMES                            │
  -- └─────────────────────────────────────────────────────┘

  theme_spec("catppuccin/nvim",               "catppuccin"),
  theme_spec("folke/tokyonight.nvim",         "tokyonight",        { opts = { style = "moon", transparent = false } }),
  theme_spec("rose-pine/neovim",              "rose-pine"),
  theme_spec("rebelot/kanagawa.nvim",         "kanagawa"),
  theme_spec("sainnhe/gruvbox-material",      "gruvbox-material",  { init = function() vim.g.gruvbox_material_better_performance = 1 end }),
  theme_spec("maxmx03/solarized.nvim",        "solarized"),
  theme_spec("craftzdog/solarized-osaka.nvim","solarized-osaka",   { opts = { transparent = false } }),

  -- ┌─────────────────────────────────────────────────────┐
  -- │                   STATUS / BUFFER                    │
  -- └─────────────────────────────────────────────────────┘

  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        theme                = "auto",
        globalstatus         = true,
        component_separators = { left = "|", right = "|" },
        section_separators   = { left = "", right = "" },
        disabled_filetypes   = { statusline = { "dashboard", "alpha" } },
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },
        lualine_x = { "encoding", "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
    },
  },

  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    opts = {
      options = {
        mode        = "buffers",
        diagnostics = "nvim_lsp",
        offsets     = { { filetype = "NvimTree", text = "Explorer", highlight = "Directory" } },
        show_buffer_close_icons = true,
        show_close_icon         = false,
        separator_style         = "slant",
      },
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                      UI ELEMENTS                     │
  -- └─────────────────────────────────────────────────────┘

  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main  = "ibl",
    opts  = {
      indent  = { char = "│" },
      scope   = { enabled = true },
      exclude = { filetypes = { "help", "lazy", "mason", "notify", "dashboard" } },
    },
  },

  {
    "rcarriga/nvim-notify",
    lazy = false,
    opts = {
      timeout   = 3000,
      stages    = "fade_in_slide_out",
      render    = "compact",
      top_down  = false,
      max_width = 60,
    },
    init = function()
      vim.notify = require("notify")
    end,
  },

  {
    "stevearc/dressing.nvim",
    lazy = true,
    opts = {},
  },

  {
    "folke/trouble.nvim",
    cmd  = "Trouble",
    opts = { use_diagnostic_signs = true },
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts  = {
      preset = "modern",
      spec   = {
        -- Core groups
        { "<leader>b",   group = "buffer" },
        { "<leader>c",   group = "code" },
        -- FIX #4: Removed orphan <leader>d group — no bindings use this prefix
        -- (DAP lives under <leader>; not <leader>d).
        { "<leader>e",   group = "explorer" },
        { "<leader>f",   group = "find" },
        -- FIX #4: Removed <leader>g "git" group — git bindings moved to <leader>.
        -- The only <leader>g usage is <leader>go* (Go language), covered below.
        { "<leader>h",   group = "harpoon" },
        { "<leader>r",   group = "run/rust" },
        { "<leader>s",   group = "split" },
        { "<leader>t",   group = "test/theme" },
        { "<leader>u",   group = "ui" },
        { "<leader>w",   group = "window" },
        { "<leader>x",   group = "utils" },
        { "<leader>,",   group = "lsp" },
        { "<leader>.",   group = "git-hunks" },
        { "<leader>;",   group = "debug" },
        { "<leader>'",   group = "run/test" },
        { "<leader>/",   group = "search/replace" },
        { "<leader>\\",  group = "terminal" },
        -- Language-specific groups
        { "<leader>py",  group = "python" },
        { "<leader>pyd", group = "python-debug" },
        { "<leader>pyr", group = "python-repl" },
        { "<leader>rb",  group = "ruby" },
        { "<leader>rs",  group = "rust" },
        { "<leader>go",  group = "go" },
        { "<leader>jv",  group = "java" },
        { "<leader>ex",  group = "elixir" },
        { "<leader>kt",  group = "kotlin" },
        { "<leader>cc",  group = "cpp/cmake" },
        { "<leader>vh",  group = "vhdl" },
        -- FIX #4: Fortran renamed fo → ft (was conflicting with Telescope <leader>fo)
        { "<leader>ft",  group = "fortran" },
        -- FIX #4: Zig prefix is <leader>z* not <leader>zz
        { "<leader>z",   group = "zig" },
        { "<leader>co",  group = "cobol" },
        { "<leader>md",  group = "markdown" },
        { "<leader>ts",  group = "typescript" },
        { "<leader>jp",  group = "js-packages" },
        { "<leader>db",  group = "database" },
        -- FIX #4: REST renamed hr → re (was mixed into Harpoon's <leader>h namespace)
        { "<leader>re",  group = "rest" },
        { "<leader>tc",  group = "test-coverage" },
      },
    },
  },

  {
    "akinsho/toggleterm.nvim",
    cmd  = "ToggleTerm",
    opts = {
      size         = 15,
      open_mapping = [[<C-\>]],
      hide_numbers = true,
      direction    = "float",
      float_opts   = { border = "curved" },
      on_open = function()
        vim.opt_local.number         = false
        vim.opt_local.relativenumber = false
      end,
    },
  },

  -- ┌─────────────────────────────────────────────────────┐
  -- │                     DASHBOARD                        │
  -- └─────────────────────────────────────────────────────┘

  {
    "nvimdev/dashboard-nvim",
    lazy     = false,
    priority = 90,
    opts = {
      theme = "doom",
      config = {
        header = {
          "",
          "  ██████╗ ███████╗ █████╗ ██╗     ██╗████████╗██╗   ██╗",
          "  ██╔══██╗██╔════╝██╔══██╗██║     ██║╚══██╔══╝╚██╗ ██╔╝",
          "  ██████╔╝█████╗  ███████║██║     ██║   ██║    ╚████╔╝ ",
          "  ██╔══██╗██╔══╝  ██╔══██║██║     ██║   ██║     ╚██╔╝  ",
          "  ██║  ██║███████╗██║  ██║███████╗██║   ██║      ██║   ",
          "  ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝╚═╝   ╚═╝      ╚═╝   ",
          "",
          "  « Reality is merely code we haven't debugged yet »",
          "",
        },
        center = {
          { icon = "  ", desc = "Find File       ", action = "Telescope find_files",                    key = "f" },
          { icon = "  ", desc = "Recent Files    ", action = "Telescope oldfiles",                      key = "r" },
          { icon = "  ", desc = "Find Text       ", action = "Telescope live_grep",                     key = "g" },
          { icon = "  ", desc = "Sessions        ", action = "SessionRestore",                          key = "s" },
          { icon = "  ", desc = "Config          ", action = "edit " .. vim.fn.stdpath("config") .. "/init.lua",  key = "c" },
          { icon = "  ", desc = "Theme           ", action = "lua require('core.theme').toggle()",      key = "t" },
          { icon = "󰒲  ", desc = "Lazy            ", action = "Lazy",                                   key = "l" },
          { icon = "  ", desc = "Quit            ", action = "qa",                                      key = "q" },
        },
        footer = function()
          local ok, lazy = pcall(require, "lazy")
          if ok then
            local s = lazy.stats()
            return { string.format("⚡ %d plugins ready", s.count) }
          end
          return {}
        end,
      },
    },
  },
}

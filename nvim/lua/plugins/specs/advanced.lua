-- nvim/lua/plugins/specs/advanced.lua
-- Advanced features: escape handling, navigation, UI enhancements, motion, icons
--
-- FIX (v2.2.4):
--   • nvim-ufo provider_selector returned {"treesitter","indent"} for every
--     buffer unconditionally. Buffers marked vim.b.large_file=true by
--     autocmds.lua have foldmethod=manual — ufo attempted to attach a
--     treesitter provider and errored silently, then left fold state broken.
--     Guard added: large_file buffers return "" (ufo detaches gracefully).
--
-- FIX (v2.3.1):
--   • nvim-ufo: close_fold_kinds renamed to close_fold_kinds_for_ft in recent
--     ufo releases. The old key was silently ignored — imports and comments were
--     never auto-closed on buffer open. Updated to the new per-filetype table
--     format: { _ = { "imports", "comment" } } which applies to all filetypes.
--     The "_" key is ufo's wildcard filetype selector.
--
-- FIX (v2.3.1b):
--   • neogen: this spec is the PRIMARY owner. cpp.lua and python.lua extend it
--     via optional=true. To avoid any possibility of the lang opts being dropped
--     when those optional specs merge, the canonical language table here now
--     includes ALL supported languages (cpp, c, python, lua, ts, js, rust, go,
--     java).
--     cpp.lua and python.lua override only their own keys — lazy merges tables
--     recursively for optional specs, so there is no conflict.
--   • neogen: <leader>xg keymap is registered here in keys= only. The duplicate
--     entry that was added to keymaps.lua in v2.3.3 has been removed from there.
--     keys= in the spec is the correct place: it both registers the map AND
--     triggers lazy-loading of the plugin on first use.
--
-- FIX (v2.3.9b):
--   • vim-matchup added. matchparen was disabled in options.lua since v2.0 with
--     no replacement. vim-matchup supersedes matchparen, adds treesitter-aware
--     multi-line matching, and improves the % motion. It sets g:loaded_matchparen
--     itself so options.lua no longer needs to suppress the builtin.
--
-- FIX (v2.3.10a):
--   • neogen languages table: java added. java.lua is a full lang spec and
--     neotest-java is registered in test.lua, but java was missing from the
--     canonical languages table despite the header claiming "ALL supported
--     languages". Added with annotation_convention = "javadoc".
--
-- FIX (v2.3.10):
--   • vim-matchup config() was calling nvim-treesitter.configs.setup() directly
--     with a minimal { matchup = { enable = true } } table. nvim-treesitter
--     treats each setup() call as a full reconfiguration — the second call from
--     vim-matchup's config() silently overwrote treesitter.lua's complete opts
--     (highlight, indent, textobjects, incremental_selection, etc.) with a
--     near-empty table depending on lazy load order. Fixed: config() removed
--     from the vim-matchup spec entirely. Instead a companion optional=true
--     nvim-treesitter spec is added that contributes only the matchup key via
--     opts=function(), which lazy.nvim merges recursively into treesitter.lua's
--     primary opts table — the correct pattern used by every other lang spec.

return {
    -- ┌─────────────────────────────────────────────────────┐
    -- │              ESCAPE & INPUT HANDLING                 │
    -- └─────────────────────────────────────────────────────┘

    {
        "max397574/better-escape.nvim",
        event = "InsertEnter",
        opts = {
            timeout          = 200,
            default_mappings = false,
            mappings = {
                i = { j = { k = "<Esc>" }, k = { j = "<Esc>" } },
            },
        },
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │        BRACKET MATCHING (replaces matchparen)        │
    -- └─────────────────────────────────────────────────────┘

    -- FIX (v2.3.9b): matchparen was disabled in options.lua since v2.0 with no
    -- replacement. vim-matchup supersedes it: treesitter-aware multi-line match
    -- highlighting, improved % motion, and it sets g:loaded_matchparen itself to
    -- prevent the builtin from loading. options.lua no longer suppresses matchparen.
    --
    -- FIX (v2.3.10): config() removed. The previous config() called
    -- nvim-treesitter.configs.setup({ matchup = { enable = true } }), which is a
    -- full reconfiguration that overwrites treesitter.lua's opts. The companion
    -- optional=true nvim-treesitter spec below correctly merges only the matchup
    -- key via opts=function() — the same pattern used by all lang/* specs.
    {
        "andymass/vim-matchup",
        event = { "BufReadPost", "BufNewFile" },
        init = function()
            -- Disable the status-line component (too noisy alongside lualine)
            vim.g.matchup_matchparen_offscreen = { method = "popup" }
            -- Defer highlight to avoid lag on large files
            vim.g.matchup_matchparen_deferred   = 1
            vim.g.matchup_matchparen_hi_surround_always = 0
            -- Let treesitter handle where possible
            vim.g.matchup_matchpref = { html = { tagnameonly = 1 } }
        end,
        -- No config() here: treesitter integration is wired through the
        -- optional nvim-treesitter spec below, which lazy merges into the
        -- primary treesitter.lua opts table without clobbering it.
    },

    -- FIX (v2.3.10): companion optional spec — contributes only the matchup
    -- enable key to the treesitter opts table via lazy's recursive merge.
    -- This is the correct way to enable a treesitter module from an external
    -- plugin spec without calling setup() a second time.
    {
        "nvim-treesitter/nvim-treesitter",
        optional = true,
        opts = function(_, opts)
            opts.matchup = opts.matchup or {}
            opts.matchup.enable = true
        end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │            SYMBOL NAVIGATION & BREADCRUMBS           │
    -- └─────────────────────────────────────────────────────┘

    {
        "SmiteshP/nvim-navic",
        event = "LspAttach",
        opts = {
            icons = {
                File          = " ", Module        = " ", Namespace     = " ",
                Package       = " ", Class         = " ", Method        = " ",
                Property      = " ", Field         = " ", Constructor   = " ",
                Enum          = " ", Interface     = " ", Function      = " ",
                Variable      = " ", Constant      = " ", String        = " ",
                Number        = " ", Boolean       = " ", Array         = " ",
                Object        = " ", Key           = " ", Null          = " ",
                EnumMember    = " ", Struct        = " ", Event         = " ",
                Operator      = " ", TypeParameter = " ",
            },
            lsp = {
                auto_attach = false,
                preference  = nil,
            },
            highlight             = true,
            separator             = " > ",
            depth_limit           = 5,
            depth_limit_indicator = "..",
            safe_output           = true,
            lazy_update_context   = false,
            click                 = false,
            format_text           = function(text) return text end,
        },
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │              COLOR & SYNTAX HIGHLIGHTING             │
    -- └─────────────────────────────────────────────────────┘

    {
        "NvChad/nvim-colorizer.lua",
        event = "BufReadPost",
        opts = {
            filetypes = { "*", "!lazy", "!lspinfo" },
            user_default_options = {
                RGB              = true,
                RRGGBB           = true,
                RRGGBBAA         = true,
                AARRGGBB         = false,
                rgb_fn           = true,
                hsl_fn           = true,
                css              = true,
                css_fn           = true,
                mode             = "background",
                tailwind         = true,
                sass             = { enable = true, parsers = { "css" } },
                virtualtext      = "■",
                virtualtext_inline = false,
                always_update    = false,
            },
            buftypes = {},
        },
        config = function(_, opts)
            require("colorizer").setup(opts)
            vim.cmd("ColorizerAttachToBuffer")
        end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │            BRACKET & DELIMITER HIGHLIGHTING          │
    -- └─────────────────────────────────────────────────────┘

    {
        "HiPhish/rainbow-delimiters.nvim",
        event = { "BufReadPre", "BufNewFile" },
        config = function()
            local rainbow_delimiters = require("rainbow-delimiters")
            require("rainbow-delimiters.setup").setup({
                strategy = {
                    [""]  = rainbow_delimiters.strategy["global"],
                    vim   = rainbow_delimiters.strategy["local"],
                    latex = rainbow_delimiters.strategy["local"],
                },
                query = {
                    [""]  = "rainbow-delimiters",
                    lua   = "rainbow-blocks",
                    latex = "rainbow-delimiters-latex",
                },
                priority = {
                    [""]  = 110,
                    lua   = 210,
                    latex = 210,
                },
                highlight = {
                    "RainbowDelimiterRed", "RainbowDelimiterYellow", "RainbowDelimiterBlue",
                    "RainbowDelimiterOrange", "RainbowDelimiterGreen", "RainbowDelimiterViolet",
                    "RainbowDelimiterCyan",
                },
                blacklist = { "html", "markdown", "text" },
            })
        end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │                MOTION & REPETITION                   │
    -- └─────────────────────────────────────────────────────┘

    { "tpope/vim-repeat", event = "VeryLazy" },

    {
        "gbprod/stay-in-place.nvim",
        event = "VeryLazy",
        opts  = {},
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │                 EDITING ENHANCEMENTS                 │
    -- └─────────────────────────────────────────────────────┘

    {
        "echasnovski/mini.align",
        lazy = true,
        keys = {
            { "ga", mode = { "n", "v" }, desc = "Align with Lua patterns" },
            { "gA", mode = { "n", "v" }, desc = "Align with Vim regex" },
        },
        config = function() require("mini.align").setup() end,
    },

    {
        "echasnovski/mini.splitjoin",
        lazy = true,
        keys = { { "gS", desc = "Toggle split/join" } },
        config = function() require("mini.splitjoin").setup() end,
    },

    {
        "echasnovski/mini.comment",
        event  = "VeryLazy",
        config = function() require("mini.comment").setup() end,
    },

    {
        "echasnovski/mini.surround",
        lazy = true,
        keys = function()
            return {
                { "gsa", desc = "Add surrounding",       mode = "v" },
                { "gsd", desc = "Delete surrounding" },
                { "gsf", desc = "Find left surrounding" },
                { "gsF", desc = "Find right surrounding" },
                { "gsh", desc = "Highlight surrounding" },
                { "gsr", desc = "Replace surrounding" },
            }
        end,
        config = function() require("mini.surround").setup() end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │                    UNDO TREE                         │
    -- └─────────────────────────────────────────────────────┘

    {
        "mbbill/undotree",
        cmd  = "UndotreeToggle",
        keys = { { "<leader>xu", "<cmd>UndotreeToggle<CR>", desc = "Undo Tree" } },
        init = function()
            vim.g.undotree_SetFocusWhenToggle = 1
            vim.g.undotree_ShortIndicators    = 1
            vim.g.undotree_WindowLayout       = 2
            vim.g.undotree_DiffpanelHeight    = 8
        end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │                  ICONS & DEVICONS                    │
    -- └─────────────────────────────────────────────────────┘

    {
        "nvim-tree/nvim-web-devicons",
        lazy = true,
        opts = {
            default     = true,
            color_icons = true,
            variant     = "default",
            strict      = true,
        },
        config = function(_, opts)
            require("nvim-web-devicons").setup(opts)
        end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │              WORD CASE OPERATORS                     │
    -- └─────────────────────────────────────────────────────┘

    { "tpope/vim-abolish", event = "VeryLazy" },

    -- ┌─────────────────────────────────────────────────────┐
    -- │                 FOLDING ENHANCEMENT                  │
    -- └─────────────────────────────────────────────────────┘

    {
        "kevinhwang91/nvim-ufo",
        event        = "VeryLazy",
        dependencies = { "kevinhwang91/promise-async" },
        opts = {
            fold_virt_text_handler = function(virtText, lnum, endLnum, width, truncate)
                local newVirtText = {}
                local suffix      = ("  %d "):format(endLnum - lnum)
                local sufWidth    = vim.fn.strdisplaywidth(suffix)
                local targetWidth = width - sufWidth
                local curWidth    = 0
                for _, chunk in ipairs(virtText) do
                    local chunkText  = chunk[1]
                    local chunkWidth = vim.fn.strdisplaywidth(chunkText)
                    if targetWidth > curWidth + chunkWidth then
                        table.insert(newVirtText, chunk)
                    else
                        chunkText = truncate(chunkText, targetWidth - curWidth)
                        table.insert(newVirtText, { chunkText, chunk[2] })
                        break
                    end
                    curWidth = curWidth + chunkWidth
                end
                table.insert(newVirtText, { suffix, "MoreMsg" })
                return newVirtText
            end,
            preview = {
                win_config = {
                    border      = { "", "─", "", "", "", "─", "", "" },
                    winhighlight = "Normal:Folded",
                    winblend    = 12,
                },
                mappings = {
                    scrollU = "<C-u>", scrollD = "<C-d>",
                    jumpTop = "[{",    jumpBot = "]}",
                },
            },
            open_fold_hl_timeout = 400,
            -- FIX (v2.3.1): close_fold_kinds renamed to close_fold_kinds_for_ft.
            close_fold_kinds_for_ft = { _ = { "imports", "comment" } },
            -- FIX (v2.2.4): guard large_file buffers.
            provider_selector = function(bufnr, _filetype, _buftype)
                if vim.b[bufnr] and vim.b[bufnr].large_file then
                    return ""
                end
                return { "treesitter", "indent" }
            end,
        },
        config = function(_, opts)
            require("ufo").setup(opts)
            vim.keymap.set("n", "zR", require("ufo").openAllFolds)
            vim.keymap.set("n", "zM", require("ufo").closeAllFolds)
            vim.keymap.set("n", "zr", require("ufo").openFoldsExceptKinds)
            vim.keymap.set("n", "zm", require("ufo").closeFoldsWith)
        end,
    },

    -- ┌─────────────────────────────────────────────────────┐
    -- │                   ANNOTATIONS                        │
    -- └─────────────────────────────────────────────────────┘

    -- FIX (v2.3.1b): PRIMARY owner of neogen. cpp.lua and python.lua extend
    -- this spec via optional=true. The languages table here covers ALL languages
    -- so setup() always runs with a complete config regardless of load order.
    -- <leader>xg lives here only — the duplicate in keymaps.lua was removed.
    {
        "danymat/neogen",
        lazy = true,
        cmd  = "Neogen",
        keys = {
            { "<leader>xg", "<cmd>Neogen<CR>", desc = "Generate docstring" },
        },
        dependencies = { "nvim-treesitter/nvim-treesitter" },
        opts = {
            snippet_engine = "luasnip",
            languages = {
                lua        = { template = { annotation_convention = "emmylua"           } },
                typescript = { template = { annotation_convention = "tsdoc"             } },
                javascript = { template = { annotation_convention = "jsdoc"             } },
                rust       = { template = { annotation_convention = "nightly"           } },
                go         = { template = { annotation_convention = "go"                } },
                cpp        = { template = { annotation_convention = "doxygen"           } },
                c          = { template = { annotation_convention = "doxygen"           } },
                python     = { template = { annotation_convention = "google_docstrings" } },
                -- FIX (v2.3.10): java added. java.lua is a full lang spec with
                -- neotest-java registered in test.lua, yet java was absent from
                -- the "ALL supported languages" table claimed by the header comment.
                java       = { template = { annotation_convention = "javadoc"           } },
            },
        },
    },
}

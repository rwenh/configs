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
            -- The old key was silently dropped by ufo — imports/comments were
            -- never auto-closed on buffer open. "_" is ufo's wildcard ft selector.
            close_fold_kinds_for_ft = { _ = { "imports", "comment" } },
            -- FIX (v2.2.4): guard large_file buffers. autocmds.lua sets foldmethod=manual
            -- on files >500KB (vim.b.large_file=true). ufo tries to attach a
            -- treesitter provider regardless and errors. Returning "" tells ufo
            -- to detach gracefully and leave the manual fold method intact.
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
                python     = { template = { annotation_convention = "google"  } },
                lua        = { template = { annotation_convention = "emmylua" } },
                typescript = { template = { annotation_convention = "tsdoc"   } },
                javascript = { template = { annotation_convention = "jsdoc"   } },
                rust       = { template = { annotation_convention = "nightly" } },
                go         = { template = { annotation_convention = "go"      } },
            },
        },
    },
}

-- ~/.config/nvim/lua/config/lsp.lua

local helpers = require("utils.helpers")

local servers = {
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        diagnostics = { globals = { "vim" } },
        workspace = {
          checkThirdParty = false,
          library = {
            vim.env.VIMRUNTIME,
          },
        },
        telemetry = { enable = false },
        completion = { callSnippet = "Replace" },
      }
    }
  },
  pyright = {
    settings = {
      python = {
        analysis = {
          typeCheckingMode = "basic",
          autoSearchPaths = true,
          useLibraryCodeForTypes = true,
          diagnosticMode = "workspace",
        }
      }
    }
  },
  rust_analyzer = {
    settings = {
      ['rust-analyzer'] = {
        cargo = {
          allFeatures = true,
          loadOutDirsFromCheck = true,
        },
        checkOnSave = {
          command = "clippy",
          extraArgs = { "--no-deps" },
        },
        procMacro = { enable = true },
        -- Enable rustfmt formatting
        rustfmt = {
          extraArgs = { "+nightly" },  -- Use nightly rustfmt if available
        },
      }
    }
  },
  ts_ls = {
    settings = {
      typescript = {
        inlayHints = {
          includeInlayParameterNameHints = "all",
          includeInlayFunctionParameterTypeHints = true,
        }
      }
    }
  },
  html = {},
  cssls = {},
  jsonls = {
    settings = {
      json = {
        schemas = require('schemastore').json.schemas(),
        validate = { enable = true },
      }
    }
  },
  yamlls = {
    settings = {
      yaml = {
        schemas = require('schemastore').yaml.schemas(),
        schemaStore = { enable = false },
      }
    }
  },
  clangd = {
    cmd = {
      "clangd",
      "--background-index",
      "--clang-tidy",
      "--header-insertion=iwyu",
      "--completion-style=detailed",
      "--function-arg-placeholders",
    },
  },
  gopls = {
    settings = {
      gopls = {
        analyses = {
          unusedparams = true,
        },
        staticcheck = true,
        gofumpt = true,
      }
    }
  },
  fortls = {},
  sqlls = {},
  marksman = {},
}

local cmp_nvim_lsp = helpers.safe_require("cmp_nvim_lsp")
if not cmp_nvim_lsp then return end

local capabilities = vim.tbl_deep_extend(
  "force",
  vim.lsp.protocol.make_client_capabilities(),
  cmp_nvim_lsp.default_capabilities()
)

-- Enable snippets
capabilities.textDocument.completion.completionItem.snippetSupport = true

local function on_attach(client, bufnr)
  local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc, silent = true })
  end

  -- Navigation
  map("n", "gd", vim.lsp.buf.definition, "Go to definition")
  map("n", "gD", vim.lsp.buf.declaration, "Go to declaration")
  map("n", "gi", vim.lsp.buf.implementation, "Go to implementation")
  map("n", "gr", vim.lsp.buf.references, "Go to references")
  map("n", "gt", vim.lsp.buf.type_definition, "Go to type definition")

  -- Documentation
  map("n", "K", vim.lsp.buf.hover, "Hover documentation")
  map("n", "<C-k>", vim.lsp.buf.signature_help, "Signature help")
  map("i", "<C-k>", vim.lsp.buf.signature_help, "Signature help")

  -- Actions
  map("n", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("v", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("n", "<leader>rn", vim.lsp.buf.rename, "Rename")
  map("n", "<leader>cf", function()
    vim.lsp.buf.format({ async = true })
  end, "Format")

  -- Workspace
  map("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, "Add workspace folder")
  map("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, "Remove workspace folder")
  map("n", "<leader>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, "List workspace folders")

  -- Disable LSP formatting if null-ls handles it
  -- Exception: Allow rust-analyzer to handle Rust formatting
  if client.name ~= "null-ls" and client.name ~= "rust_analyzer" and client.supports_method("textDocument/formatting") then
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false
  end

  -- Document highlight
  if client.server_capabilities.documentHighlightProvider then
    local group = vim.api.nvim_create_augroup("LspHighlight_" .. bufnr, { clear = true })
    vim.api.nvim_create_autocmd("CursorHold", {
      buffer = bufnr,
      group = group,
      callback = vim.lsp.buf.document_highlight
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = bufnr,
      group = group,
      callback = vim.lsp.buf.clear_references
    })
  end

  -- Inlay hints (Neovim 0.10+)
  if vim.lsp.inlay_hint and client.supports_method("textDocument/inlayHint") then
    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    map("n", "<leader>ch", function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr }), { bufnr = bufnr })
    end, "Toggle inlay hints")
  end

  -- Codelens (if supported)
  if client.supports_method("textDocument/codeLens") then
    vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave" }, {
      buffer = bufnr,
      callback = vim.lsp.codelens.refresh,
    })
    map("n", "<leader>cl", vim.lsp.codelens.run, "Run codelens")
  end
end

-- Setup servers with proper API detection
local nvim_version = vim.version()
local use_new_api = nvim_version.minor >= 11

for server, config in pairs(servers) do
  local server_config = vim.tbl_extend("force", {
    capabilities = capabilities,
    on_attach = on_attach,
  }, config)

  -- FIXED: Proper 0.11 API usage
  if use_new_api then
    local ok = pcall(function()
      vim.lsp.config(server, server_config)
      vim.lsp.enable(server)
    end)
    if not ok then
      vim.notify("Failed to setup " .. server .. " with new API", vim.log.levels.WARN)
    end
  else
    -- Use lspconfig for older versions
    local lspconfig = helpers.safe_require("lspconfig")
    if lspconfig and lspconfig[server] then
      pcall(function()
        lspconfig[server].setup(server_config)
      end)
    end
  end
end

-- Diagnostics configuration
vim.diagnostic.config({
  virtual_text = {
    prefix = "●",
    source = "if_many",
    spacing = 4,
  },
  float = {
    border = "rounded",
    source = "always",
    header = "",
    prefix = "",
    format = function(diagnostic)
      return string.format("%s (%s)", diagnostic.message, diagnostic.source)
    end,
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

-- Diagnostic signs
local signs = {
  Error = " ",
  Warn = " ",
  Hint = "󰌵 ",
  Info = " "
}
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- LSP UI customization
require("lspconfig.ui.windows").default_options.border = "rounded"

-- Floating window borders
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
  vim.lsp.handlers.hover,
  { border = "rounded" }
)

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
  vim.lsp.handlers.signature_help,
  { border = "rounded" }
)

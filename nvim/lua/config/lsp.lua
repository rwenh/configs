-- ~/.config/nvim/lua/config/lsp.lua

local helpers = require("utils.helpers")

local servers = {
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        diagnostics = { globals = { "vim" } },
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
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
        }
      }
    }
  },
  rust_analyzer = {
    settings = {
      ['rust-analyzer'] = {
        cargo = { allFeatures = true },
        checkOnSave = { command = "clippy" },
      }
    }
  },
  ts_ls = {},
  html = {},
  cssls = {},
  jsonls = {},
  yamlls = {},
  clangd = {},
  gopls = {},
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

local function on_attach(client, bufnr)
  local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc, silent = true })
  end
  
  map("n", "gd", vim.lsp.buf.definition, "Definition")
  map("n", "gD", vim.lsp.buf.declaration, "Declaration")
  map("n", "gi", vim.lsp.buf.implementation, "Implementation")
  map("n", "gr", vim.lsp.buf.references, "References")
  map("n", "K", vim.lsp.buf.hover, "Hover")
  map("n", "<C-k>", vim.lsp.buf.signature_help, "Signature")
  map("i", "<C-k>", vim.lsp.buf.signature_help, "Signature")
  map("n", "<leader>ca", vim.lsp.buf.code_action, "Code Action")
  map("v", "<leader>ca", vim.lsp.buf.code_action, "Code Action")
  map("n", "<leader>rn", vim.lsp.buf.rename, "Rename")
  map("n", "<leader>cf", function() vim.lsp.buf.format({ async = true }) end, "Format")
  
  -- Document highlight
  if client.server_capabilities.documentHighlightProvider then
    local group = vim.api.nvim_create_augroup("LspHighlight_" .. bufnr, { clear = true })
    vim.api.nvim_create_autocmd("CursorHold", {
      buffer = bufnr, group = group, callback = vim.lsp.buf.document_highlight
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      buffer = bufnr, group = group, callback = vim.lsp.buf.clear_references
    })
  end
  
  -- Inlay hints (Neovim 0.10+)
  if client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint then
    vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
  end
end

-- Setup servers
local nvim_version = vim.version()
local use_new_api = nvim_version.minor >= 11

for server, config in pairs(servers) do
  local server_config = vim.tbl_extend("force", {
    capabilities = capabilities,
    on_attach = on_attach,
  }, config)
  
  if use_new_api then
    pcall(function()
      vim.lsp.config[server] = server_config
      vim.lsp.enable(server)
    end)
  else
    local lspconfig = helpers.safe_require("lspconfig")
    if lspconfig and lspconfig[server] then
      pcall(function() lspconfig[server].setup(server_config) end)
    end
  end
end

-- Diagnostics
vim.diagnostic.config({
  virtual_text = {
    prefix = "●",
    source = "if_many",
  },
  float = {
    border = "rounded",
    source = "always",
    header = "",
    prefix = "",
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

local signs = { Error = " ", Warn = " ", Hint = "󰌵 ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- LSP UI customization
require("lspconfig.ui.windows").default_options.border = "rounded"
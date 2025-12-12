-- ~/.config/nvim/lua/config/lsp.lua
-- LSP server configuration (Neovim 0.11+ compatible)

local helpers = require("utils.helpers")

local function get_lsp_servers()
  return {
    pyright = { settings = { python = { analysis = { typeCheckingMode = "basic" } } } },
    rust_analyzer = { settings = { ['rust-analyzer'] = { cargo = { allFeatures = true }, checkOnSave = { command = "clippy" } } } },
    fortls = { settings = { fortls = { nthreads = 2 } } },
    sqlls = { settings = {} },
    vhdl_ls = { settings = {} },
    html = { settings = {} },
    cssls = { settings = {} },
    jdtls = { cmd = { "jdtls" } },
    lua_ls = { settings = { Lua = { runtime = { version = "LuaJIT" }, diagnostics = { globals = { "vim" } }, workspace = { checkThirdParty = false } } } },
    jsonls = { settings = {} },
    yamlls = { settings = {} },
    clangd = { settings = {} },
    marksman = { settings = {} },
  }
end

local cmp_nvim_lsp = helpers.safe_require("cmp_nvim_lsp")
if not cmp_nvim_lsp then return end

local capabilities = vim.tbl_deep_extend("force", {}, vim.lsp.protocol.make_client_capabilities(), cmp_nvim_lsp.default_capabilities())

local function on_attach(client, bufnr)
  local function map(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc, silent = true })
  end
  
  map("n", "gd", vim.lsp.buf.definition, "Go to definition")
  map("n", "gD", vim.lsp.buf.declaration, "Go to declaration")
  map("n", "gi", vim.lsp.buf.implementation, "Go to implementation")
  map("n", "gr", vim.lsp.buf.references, "Go to references")
  map("n", "K", vim.lsp.buf.hover, "Hover documentation")
  map("n", "<C-k>", vim.lsp.buf.signature_help, "Signature help")
  map("i", "<C-k>", vim.lsp.buf.signature_help, "Signature help")
  map("n", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("v", "<leader>ca", vim.lsp.buf.code_action, "Code action")
  map("n", "<leader>rn", vim.lsp.buf.rename, "Rename")
  map("n", "<leader>cf", function() vim.lsp.buf.format({ async = true }) end, "Format")
  
  if client.server_capabilities.documentHighlightProvider then
    local grp = vim.api.nvim_create_augroup("LspDocumentHighlight", { clear = false })
    vim.api.nvim_create_autocmd({ "CursorHold" }, {
      buffer = bufnr, group = grp, callback = vim.lsp.buf.document_highlight,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved" }, {
      buffer = bufnr, group = grp, callback = vim.lsp.buf.clear_references,
    })
  end
end

-- Check Neovim version for API compatibility
local nvim_version = vim.version()
local use_new_api = (nvim_version.major > 0) or (nvim_version.major == 0 and nvim_version.minor >= 11)

local servers = get_lsp_servers()

-- Setup LSP servers based on Neovim version
if use_new_api then
  -- Neovim 0.11+ - Use new vim.lsp.config API
  for server, config in pairs(servers) do
    local server_config = vim.tbl_extend("force", { 
      capabilities = capabilities, 
      on_attach = on_attach 
    }, config)
    
    pcall(function()
      vim.lsp.config[server] = server_config
      vim.lsp.enable(server)
      _G.nvim_ide.language_servers[server] = true
    end)
  end
else
  -- Neovim < 0.11 - Use legacy lspconfig
  local lspconfig = helpers.safe_require("lspconfig")
  if not lspconfig then return end
  
  for server, config in pairs(servers) do
    local server_config = vim.tbl_extend("force", { 
      capabilities = capabilities, 
      on_attach = on_attach 
    }, config)
    
    pcall(function()
      if lspconfig[server] then 
        lspconfig[server].setup(server_config)
        _G.nvim_ide.language_servers[server] = true
      end
    end)
  end
end

-- Auto-detect Deno vs ts_ls (formerly tsserver)
if helpers.project_has_file("deno.json") or helpers.project_has_file("deno.jsonc") then
  -- Use Deno LSP
  local deno_config = {
    root_dir = function(fname)
      local util = require("lspconfig.util")
      return util.root_pattern("deno.json", "deno.jsonc")(fname)
    end,
    on_attach = on_attach,
    capabilities = capabilities,
    init_options = {
      enable = true,
      lint = true,
      unstable = true,
    }
  }
  
  if use_new_api then
    pcall(function()
      vim.lsp.config.denols = deno_config
      vim.lsp.enable("denols")
      _G.nvim_ide.language_servers.denols = true
    end)
  else
    local lspconfig = helpers.safe_require("lspconfig")
    if lspconfig and lspconfig.denols then
      pcall(function()
        lspconfig.denols.setup(deno_config)
        _G.nvim_ide.language_servers.denols = true
      end)
    end
  end
else
  -- Use ts_ls (formerly tsserver)
  local ts_config = {
    on_attach = on_attach,
    capabilities = capabilities,
    init_options = {
      preferences = {
        disableSuggestions = false,
      }
    }
  }
  
  if use_new_api then
    pcall(function()
      vim.lsp.config.ts_ls = ts_config
      vim.lsp.enable("ts_ls")
      _G.nvim_ide.language_servers.ts_ls = true
    end)
  else
    local lspconfig = helpers.safe_require("lspconfig")
    if lspconfig and lspconfig.ts_ls then
      pcall(function()
        lspconfig.ts_ls.setup(ts_config)
        _G.nvim_ide.language_servers.ts_ls = true
      end)
    end
  end
end

-- Diagnostic configuration
vim.diagnostic.config({
  virtual_text = { prefix = "◆", source = "if_many" },
  float = { border = "rounded", source = "always" },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

-- Diagnostic signs
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

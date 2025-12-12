-- ~/.config/nvim/lua/config/null-ls.lua

local null_ls = require("null-ls")
local b = null_ls.builtins

null_ls.setup({
  sources = {
    -- Python
    b.formatting.black.with({ extra_args = { "--fast", "--line-length", "88" } }),
    b.formatting.isort,
    b.diagnostics.ruff,
    
    -- Lua
    b.formatting.stylua,
    
    -- JavaScript/TypeScript
    b.formatting.prettier.with({
      filetypes = { "javascript", "typescript", "javascriptreact", "typescriptreact", "css", "html", "json", "yaml", "markdown" },
    }),
    
    -- Shell
    b.formatting.shfmt,
    b.diagnostics.shellcheck,
    
    -- C/C++
    b.formatting.clang_format,
    
    -- SQL
    b.formatting.sql_formatter,
    
    -- Git
    b.code_actions.gitsigns,
  },
  
  on_attach = function(client, bufnr)
    if client.supports_method("textDocument/formatting") then
      local group = vim.api.nvim_create_augroup("LspFormatting", { clear = false })
      vim.api.nvim_clear_autocmds({ group = group, buffer = bufnr })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = group,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({
            filter = function(c)
              return c.name == "null-ls"
            end,
            bufnr = bufnr,
          })
        end,
      })
    end
  end,
  
  border = "rounded",
  debug = false,
})
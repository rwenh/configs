-- ~/.config/nvim/lua/config/null-ls.lua
-- null-ls configuration for formatters and linters

local helpers = require("utils.helpers")

helpers.setup_plugin("null-ls", function(null_ls)
  local b = null_ls.builtins
  
  null_ls.setup({
    sources = {
      -- Python
      b.formatting.black.with({
        extra_args = { "--fast" }
      }),
      b.formatting.isort,
      b.diagnostics.ruff,  -- Updated: ruff instead of flake8 (faster, modern)
      
      -- Fortran
      b.formatting.fprettify,
      
      -- SQL
      b.formatting.sql_formatter,
      b.formatting.sqlfluff.with({
        extra_args = { "--dialect", "postgres" }
      }),
      
      -- Web
      b.formatting.prettier.with({
        filetypes = { "html", "css", "javascript", "typescript", "json", "yaml", "markdown" },
        extra_args = { "--single-quote", "--trailing-comma", "all" }
      }),
      
      -- Java
      b.formatting.google_java_format,
      
      -- Lua
      b.formatting.stylua.with({
        extra_args = { "--indent-type", "Spaces", "--indent-width", "2" }
      }),
      
      -- C/C++
      b.formatting.clang_format.with({
        extra_args = { "--style", "{ BasedOnStyle: LLVM, IndentWidth: 4 }" }
      }),
      
      -- Shell
      b.formatting.shfmt.with({
        extra_args = { "-i", "2", "-ci" }
      }),
      b.diagnostics.shellcheck,
      
      -- Git
      b.code_actions.gitsigns,
    },
    
    on_attach = function(client, bufnr)
      if client.supports_method("textDocument/formatting") then
        local augroup = vim.api.nvim_create_augroup("LspFormatting", { clear = false })
        vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        vim.api.nvim_create_autocmd("BufWritePre", {
          group = augroup,
          buffer = bufnr,
          callback = function()
            vim.lsp.buf.format({
              filter = function(client)
                return client.name == "null-ls"
              end,
              bufnr = bufnr,
            })
          end,
        })
      end
    end,
  })
end)

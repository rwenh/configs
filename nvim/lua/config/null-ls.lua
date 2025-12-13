-- ~/.config/nvim/lua/config/null-ls.lua

local null_ls = require("null-ls")
local b = null_ls.builtins

null_ls.setup({
  sources = {
    -- Python
    b.formatting.black.with({ extra_args = { "--fast", "--line-length", "88" } }),
    b.formatting.isort,

    -- Lua
    b.formatting.stylua,

    -- JavaScript/TypeScript
    b.formatting.prettier.with({
      filetypes = {
        "javascript", "typescript", "javascriptreact", "typescriptreact",
        "css", "scss", "html", "json", "jsonc", "yaml", "markdown",
        "markdown.mdx", "graphql", "handlebars"
      },
    }),

    -- Shell
    b.formatting.shfmt,

    -- C/C++
    b.formatting.clang_format,

    -- Go
    b.formatting.gofmt,
    b.formatting.goimports,

    -- Rust (use rust_analyzer LSP for formatting instead)
    -- rustfmt is not a builtin in none-ls, rust-analyzer handles it

    -- SQL
    b.formatting.sql_formatter,

    -- Git
    b.code_actions.gitsigns,
  },

  on_attach = function(client, bufnr)
    -- Format on save for this buffer
    if client.supports_method("textDocument/formatting") then
      local augroup = vim.api.nvim_create_augroup("NullLsFormatting_" .. bufnr, { clear = true })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = augroup,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({
            filter = function(c)
              return c.name == "null-ls"
            end,
            bufnr = bufnr,
            timeout_ms = 2000,
          })
        end,
      })

      -- User command for manual formatting
      vim.api.nvim_buf_create_user_command(bufnr, "Format", function()
        vim.lsp.buf.format({
          filter = function(c) return c.name == "null-ls" end,
          bufnr = bufnr,
        })
      end, { desc = "Format with null-ls" })
    end
  end,

  border = "rounded",
  debug = false,
})

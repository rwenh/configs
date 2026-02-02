# INSTALLATION INSTRUCTIONS

## What Was Fixed

1. **LSP Setup** - Fixed `vim.lsp.enable()` → `lspconfig[server].setup(config)`
2. **Format on Save** - Removed duplicate autocmd (using Conform only)
3. **Harpoon** - Removed duplicate keymaps from editor.lua
4. **Gitsigns** - Unified keymaps (removed from keymaps.lua)
5. **Better-Escape** - Removed plugin (using native keymaps)
6. **Emmet** - Changed from `Ctrl+e` to `Ctrl+y,` to avoid conflicts
7. **LSP Keymaps** - Removed from keymaps.lua (kept in lsp.lua only)

## Installation Steps

1. **Backup your current config:**
   ```bash
   mv ~/.config/nvim ~/.config/nvim.backup
   ```

2. **Copy the fixed config:**
   ```bash
   # Extract all files from the outputs directory
   # They should go into ~/.config/nvim/
   ```

3. **File structure should be:**
   ```
   ~/.config/nvim/
   ├── init.lua
   └── lua/
       ├── core/
       │   ├── autocmds.lua
       │   ├── bootstrap.lua
       │   ├── commands.lua
       │   ├── keymaps.lua
       │   ├── options.lua
       │   ├── theme.lua
       │   └── util/
       │       ├── path.lua
       │       └── runner.lua
       └── plugins/
           ├── init.lua
           └── specs/
               ├── init.lua
               ├── advanced.lua
               ├── completion.lua
               ├── dap.lua
               ├── editor.lua
               ├── git.lua
               ├── lsp.lua
               ├── test.lua
               ├── treesitter.lua
               ├── ui.lua
               └── lang/
                   ├── c.lua
                   ├── cobol.lua
                   ├── cpp.lua
                   ├── css.lua
                   ├── database.lua
                   ├── elixir.lua
                   ├── fortran.lua
                   ├── go.lua
                   ├── html.lua
                   ├── java.lua
                   ├── javascript.lua
                   ├── kotlin.lua
                   ├── markdown.lua
                   ├── python.lua
                   ├── rest.lua
                   ├── ruby.lua
                   ├── rust.lua
                   ├── sql.lua
                   ├── typescript.lua
                   ├── vhdl.lua
                   ├── web.lua
                   └── zig.lua
   ```

4. **Start Neovim:**
   ```bash
   nvim
   ```
   
   Lazy.nvim will automatically install all plugins.

5. **Install LSP servers and tools:**
   ```vim
   :MasonInstallAll
   ```

6. **Verify everything works:**
   ```vim
   :checkhealth
   ```

## Key Changes Summary

- All keymaps now in ONE place: `lua/core/keymaps.lua`
- No duplicate LSP keymaps
- No duplicate format-on-save
- No conflicting Harpoon keymaps
- Emmet uses `Ctrl+y,` instead of `Ctrl+e`
- LSP servers properly set up with `lspconfig[server].setup()`
- Better-escape plugin removed (native `jk`/`kj` works fine)

## Troubleshooting

If you see errors:
1. Delete `~/.local/share/nvim` (plugins)
2. Delete `~/.cache/nvim` (cache)
3. Restart Neovim
4. Run `:MasonInstallAll`

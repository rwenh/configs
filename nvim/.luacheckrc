-- .luacheckrc — static checks for the Lua in this repository (see scripts/check.sh).
std = "luajit"
globals = { "vim" }
max_line_length = false
-- Unused function arguments are normal in callbacks and spec factories.
unused_args = false

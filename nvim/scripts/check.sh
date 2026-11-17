#!/bin/sh
# scripts/check.sh — repository checks that need no plugin installs and no network.
# Run from anywhere:  sh scripts/check.sh
# Exits non-zero on the first failing check.
set -eu

cd "$(dirname "$0")/.."

echo "== stylua --check"
if ! command -v stylua >/dev/null 2>&1; then
  echo "stylua is not installed (cargo install stylua)" >&2
  exit 1
fi
stylua --check .

echo "== every Lua file parses"
nvim --headless -u NONE "+lua local bad = 0; for _, f in ipairs(vim.fn.globpath('.', '**/*.lua', false, true)) do local _, e = loadfile(f); if e then bad = bad + 1; print(e) end end; print(bad .. ' file(s) do not parse'); vim.cmd(bad == 0 and 'qa!' or 'cquit 1')"

echo "== MANIFEST.md generated tables are current"
GEN_DOCS=check nvim --headless -u NONE "+luafile scripts/gen_docs.lua"

echo "all checks passed"

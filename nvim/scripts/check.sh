#!/bin/sh
# scripts/check.sh — every repository check that needs no plugin install and no network.
#
#   sh scripts/check.sh
#
# Tools: stylua (pinned in .stylua-version), luacheck, nvim 0.11+. Exits non-zero on the first failing check.
# StyLua is taken from $STYLUA, then from Mason's bin directory, then from PATH.
set -eu

cd "$(dirname "$0")/.."

# ── stylua: the pinned version ─────────────────────────────────────────────────
PINNED=$(cat .stylua-version)
DATA="${XDG_DATA_HOME:-$HOME/.local/share}/nvim"
if [ -n "${STYLUA:-}" ]; then
  :
elif [ -x "$DATA/mason/bin/stylua" ]; then
  STYLUA="$DATA/mason/bin/stylua"
elif command -v stylua >/dev/null 2>&1; then
  STYLUA=stylua
else
  echo "stylua not found. Install it with :MasonInstall stylua, or set STYLUA=/path/to/stylua." >&2
  exit 1
fi
GOT=$("$STYLUA" --version | awk '{print $2}')
if [ "$GOT" != "$PINNED" ]; then
  echo "stylua $GOT is in use, but .stylua-version pins $PINNED. Install $PINNED or update the pin." >&2
  exit 1
fi

for tool in nvim luacheck; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "$tool not found on PATH." >&2
    exit 1
  fi
done

echo "== stylua $GOT --check"
"$STYLUA" --check .

echo "== luacheck (static analysis: undefined globals, unused variables)"
luacheck . --no-color

echo "== every Lua file parses"
nvim --headless -u NONE "+lua local bad = 0; for _, f in ipairs(vim.fn.globpath('.', '**/*.lua', false, true)) do local _, e = loadfile(f); if e then bad = bad + 1; print(e) end end; print(bad .. ' file(s) do not parse'); vim.cmd(bad == 0 and 'qa!' or 'cquit 1')"

echo "== unit tests (pure modules)"
nvim --headless -u NONE -l tests/run.lua

echo "== structure: requires, spec registry, escape hatches"
nvim --headless -u NONE -l scripts/check_graph.lua

echo "== docs and lock: KEYMAPS.md bindings, lazy-lock.json and specs"
nvim --headless -u NONE -l scripts/check_docs.lua

echo "== MANIFEST.md generated tables are current"
GEN_DOCS=check nvim --headless -u NONE "+luafile scripts/gen_docs.lua"

echo "all checks passed"

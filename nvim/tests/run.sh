#!/usr/bin/env bash
# tests/run.sh — run every headless test against THIS config in an isolated XDG sandbox.
# Needs: nvim >= 0.11, git, gcc, network on first run (plugin + parser install).
# Env: SMOKE_SANDBOX (default /tmp/nvim-smoke)
set -u
HERE="$(cd "$(dirname "$0")/.." && pwd)"
SB="${SMOKE_SANDBOX:-/tmp/nvim-smoke}"
mkdir -p "$SB/config" "$SB/data" "$SB/state" "$SB/cache"
ln -sfn "$HERE" "$SB/config/nvim"
export XDG_CONFIG_HOME="$SB/config" XDG_DATA_HOME="$SB/data" XDG_STATE_HOME="$SB/state" XDG_CACHE_HOME="$SB/cache"

nvim --headless "+Lazy! sync" "+qa" >/dev/null 2>&1
nvim --headless "+TSUpdateSync" "+qa" >/dev/null 2>&1

rc=0
for t in specs_lint modules units smoke keymaps; do
  echo "── $t"
  nvim --headless "+luafile $HERE/tests/$t.lua" 2>&1 | grep -a -E "^(==|  |specs_lint|modules|units|documented|MISSING)" || true
  [ "${PIPESTATUS[0]}" -eq 0 ] || rc=1
done
echo "── docs"
GEN_DOCS=check nvim --headless "+luafile $HERE/scripts/gen_docs.lua" 2>&1 | grep -a -E "gen_docs|STALE" || true
[ "${PIPESTATUS[0]}" -eq 0 ] || rc=1
exit $rc

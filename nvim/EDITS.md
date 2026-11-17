# Recalibration: small edits

These files change in a few places only. Apply each replacement exactly as written. The full files
are alongside this document: `lua/core/util/path.lua`, `lua/core/util/lspreg.lua`,
`lua/plugins/specs/lsp.lua`, `lua/plugins/specs/dap.lua`, `lua/plugins/specs/workflow.lua`,
`lua/plugins/specs/lang/shared.lua`, `lua/plugins/specs/lang/css.lua`, `init.lua`, `scripts/check.sh`.

---

## 1. `lua/core/lazy.lua` (A12)

Delete the `already_imported_lang_stems` function and the `local ROOT = ...` line. Replace `collect_specs` with:

```lua
local function collect_specs()
  return safe_spec("plugins.specs")
end
```

Update the header: the spec list is `plugins/specs/init.lua`. A language file that is not imported there is not loaded.

## 2. `scripts/gen_docs.lua` (A11, A12)

Replace the header paragraph that begins `-- Reads the sources directly` with:

```lua
-- Reads lua/core/flags.lua for escape hatches and loads the spec modules named in plugins/specs/init.lua
-- for plugins. It does not call lazy.nvim, install anything, or touch the network. Loading a spec module runs
-- its file-scope code, so that code must not depend on editor state (buffers, loaded plugins).
-- Versions come from lazy-lock.json and are not part of these tables.
```

Replace the whole `spec_modules` function, including its comment, with:

```lua
local function spec_modules()
  local ok, registry = pcall(require, "plugins.specs")
  if not ok then
    fail("cannot load the spec registry: " .. tostring(registry))
  end
  local mods, seen = {}, {}
  for _, e in ipairs(registry) do
    if type(e) == "table" and type(e.import) == "string" and not seen[e.import] then
      seen[e.import] = true
      table.insert(mods, e.import)
    end
  end
  return mods
end
```

## 3. `lua/plugins/specs/lang/ruby.lua` (A11)

Delete this block at file scope (it ran during spec collection):

```lua
for _, b in ipairs(vim.api.nvim_list_bufs()) do -- buffers that already exist when this spec loads
  if vim.api.nvim_buf_is_loaded(b) and vim.bo[b].filetype == "ruby" then
    sync_rails_keymaps(b)
  end
end
```

Replace the vim-rails entry

```lua
  { "tpope/vim-rails", ft = { "ruby", "eruby" } },
```

with

```lua
  {
    "tpope/vim-rails",
    ft = { "ruby", "eruby" },
    config = function()
      -- Ruby buffers opened before this plugin loaded get their Rails keymaps now.
      for _, b in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(b) and vim.bo[b].filetype == "ruby" then
          sync_rails_keymaps(b)
        end
      end
    end,
  },
```

## 4. `lua/core/util/dburl.lua` (A3)

Replace `M.project_root` with:

```lua
function M.project_root(buf)
  local ok_name, name = pcall(vim.api.nvim_buf_get_name, buf or 0)
  local dir = (ok_name and name ~= "") and vim.fn.fnamemodify(name, ":p:h") or vim.fn.getcwd()
  return require("core.util.path").find_root(dir) or dir
end
```

## 5. `lua/plugins/specs/lang/java.lua` (A3)

Delete the `ROOT_MARKERS` table. In `attach`, replace

```lua
  local root = require("core.util.path").find_up(ROOT_MARKERS, dir) or dir
```

with

```lua
  local root = require("core.util.path").find_root(dir) or dir
```

## 6. `lua/core/util/exec.lua` (A14)

Replace `M.version_async` with the version below. The pre-0.10 fallback is removed, since the minimum is Neovim 0.11.

```lua
function M.version_async(bin, cmd, callback, pattern)
  if type(callback) ~= "function" then
    return
  end

  local key = resolve(bin)
  local cached = _version_cache[key]
  if cached ~= nil then
    vim.schedule(function()
      callback(cached ~= NOT_INSTALLED and cached or nil)
    end)
    return
  end

  if vim.fn.executable(bin) ~= 1 then
    _version_cache[key] = NOT_INSTALLED
    vim.schedule(function()
      callback(nil)
    end)
    return
  end

  vim.system(cmd, { text = true }, function(result)
    local ver = "installed"
    if result.code == 0 and result.stdout and result.stdout ~= "" then
      local first = vim.trim(result.stdout:match("([^\n]+)") or "")
      ver = first:match(pattern or "%d+%.%d+%.?%d*") or "installed"
    end
    _version_cache[key] = ver
    vim.schedule(function()
      callback(ver)
    end)
  end)
end
```

## 7. Setup calls that swallowed errors (A10)

**`lua/plugins/specs/advanced.lua`**, nvim-ufo `config`. Replace the start of the function, through the `ufo.setup` call:

```lua
    config = function(_, opts)
      local ufo = require("ufo")
      require("core.util.safe_setup").run("nvim-ufo", function()
        ufo.setup(opts)
      end, "run :Lazy update nvim-ufo")
```

Keep the four `vim.keymap.set` lines that follow.

**`lua/plugins/specs/editor.lua`**, flash `config`. Replace the function with:

```lua
    config = function(_, opts)
      require("core.util.safe_setup").run("flash", function()
        require("flash").setup(opts)
      end, "run :Lazy update flash.nvim")
    end,
```

**`lua/plugins/specs/editor.lua`**, template `config`. Change the wrapper around `template.setup({ ... })` from `pcall(function()` to `require("core.util.safe_setup").run("template", function()`, and its closing `end)` to:

```lua
      end, "run :Lazy update template.nvim")
```

**`lua/plugins/specs/hud.lua`**, barbecue `config`. Replace

```lua
      pcall(function()
        barbecue.setup(opts)
      end)
```

with

```lua
      require("core.util.safe_setup").run("barbecue", function()
        barbecue.setup(opts)
      end, "run :Lazy update barbecue.nvim")
```

**`lua/plugins/specs/git.lua`**, octo `config`. Replace

```lua
      require("core.util.safe_setup").run("octo", function()
      pcall(function()
        octo.setup(opts)
      end)
    end)
```

with

```lua
      require("core.util.safe_setup").run("octo", function()
        octo.setup(opts)
      end, "run :Lazy update octo.nvim")
```

## 8. Runner project file: documented but never loaded (top-five item 2)

`.runner.lua` was documented in five places and no code reads it. Per-project commands belong in `.overseer.lua`,
which `runner.lua` already states. The documentation is corrected rather than adding a loader.

- `lua/core/flags.lua`, `disable_project_configs` effect becomes:
  `Never load per-project Lua files (`.lspconfig.lua`, `.nvim-dap.lua`, `.overseer.lua`); otherwise they load only after you `:trust` them`
- `lua/core/util/secure.lua`, header: delete `.runner.lua, ` from the list of project files.
- `README.md`, the "Per-project overrides" bullet: delete `` `.runner.lua`, ``.
- `MANIFEST.md`, last paragraph: delete `` `.runner.lua`, ``.

## 9. Regenerate the MANIFEST tables

The flags table comes from `flags.lua`, so regenerate after the edits above:

```sh
nvim --headless -u NONE "+luafile scripts/gen_docs.lua"
sh scripts/check.sh
```

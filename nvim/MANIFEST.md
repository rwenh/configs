# nvim-ide config modules — manifest

Assembled from the upload (placed by each file's path header), the batch overlays (final versions),
and two files rebuilt from the earlier document text. Each module passes `luac -p`.
Verified: `units.lua` 0 failures; `modules.lua` 62 loaded, 0 failures (headless Neovim 0.11.4).

Not in this bundle (next phase): `tests/`, `README.md`, `CHANGELOG.md`, `KEYMAPS.md`.

Load order is in `init.lua`. Module count: 62 under `lua/`, plus `init.lua`.

| File | Provenance | sha256 (16) |
|---|---|---|
| `init.lua` | reconstructed from earlier document text (no upload copy) | `efc26ea0ba8eedef` |
| `lazy-lock.json` | upload (root) | `37903166cfe2b3c8` |
| `lua/core/autocmds.lua` | overlay batch1 (final version) | `53fbb98f103a4e74` |
| `lua/core/bootstrap.lua` | upload | `e0adb42d165dfe33` |
| `lua/core/commands.lua` | upload | `de8023694d9054a4` |
| `lua/core/flags.lua` | overlay batch1 (final version) | `1a29d8f3e09550dc` |
| `lua/core/focus.lua` | upload | `af0f3020a85566c0` |
| `lua/core/health.lua` | overlay batch6 (final version) | `703bbed2b407462e` |
| `lua/core/highlights.lua` | upload | `4364f74e307c85d4` |
| `lua/core/keymaps.lua` | upload | `4720a051c7fc8f62` |
| `lua/core/lazy.lua` | upload | `fc516ae2e3d2522a` |
| `lua/core/options.lua` | upload | `9f55cc8dd7519974` |
| `lua/core/theme.lua` | upload | `bf23d3fd292b7c2c` |
| `lua/core/util/buf_keymap.lua` | upload | `61c39bedaeec210e` |
| `lua/core/util/exec.lua` | overlay batch2 (final version) | `adb798e481249a36` |
| `lua/core/util/format.lua` | upload | `7e259474d3dc77b3` |
| `lua/core/util/icons.lua` | upload | `a6832ac0ae36efac` |
| `lua/core/util/jsonc.lua` | upload | `2f1e323dc0dc0509` |
| `lua/core/util/jvm.lua` | upload | `ec7676591df9d4a2` |
| `lua/core/util/limits.lua` | overlay batch1 (final version) | `3cf3ee4149022e02` |
| `lua/core/util/mason.lua` | overlay batch2 (final version) | `54bb575d5402af07` |
| `lua/core/util/packages.lua` | upload | `b240d421baada0ef` |
| `lua/core/util/path.lua` | upload | `08c6877ca4244f3b` |
| `lua/core/util/quotes.lua` | upload | `f7af2f4892fbb000` |
| `lua/core/util/reload.lua` | upload | `072237d292eb51aa` |
| `lua/core/util/runner.lua` | overlay batch3 (final version) | `1a8f6e1ad8d9f2b8` |
| `lua/core/util/secure.lua` | upload | `6f2c5c3d2348d542` |
| `lua/core/util/snippets.lua` | upload | `db0e00182f88422d` |
| `lua/core/util/term.lua` | upload | `3e9c1636da809c75` |
| `lua/core/util/vhdl.lua` | reconstructed from earlier document text (no upload copy) | `530a3ac3ad26f653` |
| `lua/plugins/specs/advanced.lua` | upload | `770957d1784794b2` |
| `lua/plugins/specs/completion.lua` | upload | `2c57d17c75472ec8` |
| `lua/plugins/specs/dap.lua` | upload | `30435911d7ec8453` |
| `lua/plugins/specs/editor.lua` | upload | `68a4adb5800ae20c` |
| `lua/plugins/specs/git.lua` | upload | `d25a481b4dfeda27` |
| `lua/plugins/specs/hud.lua` | upload | `95dd12073484ec3b` |
| `lua/plugins/specs/init.lua` | upload | `3d2eca64f102305d` |
| `lua/plugins/specs/lang/c.lua` | upload | `67aa226b3567c33d` |
| `lua/plugins/specs/lang/cobol.lua` | upload | `a9ef2b65eaefd76d` |
| `lua/plugins/specs/lang/cpp.lua` | upload | `62b4138f586c52ba` |
| `lua/plugins/specs/lang/css.lua` | upload | `9aa574ac90cac2bf` |
| `lua/plugins/specs/lang/database.lua` | upload | `e62fd08ae1ad677a` |
| `lua/plugins/specs/lang/elixir.lua` | upload | `1b1945c02894323a` |
| `lua/plugins/specs/lang/fortran.lua` | upload | `8fbcfadf287b391f` |
| `lua/plugins/specs/lang/go.lua` | upload | `dc951c5c32e95b55` |
| `lua/plugins/specs/lang/html.lua` | upload | `053cf13667151fce` |
| `lua/plugins/specs/lang/java.lua` | upload | `fe0f916a9608e65e` |
| `lua/plugins/specs/lang/javascript.lua` | upload | `75e055875f7c8768` |
| `lua/plugins/specs/lang/kotlin.lua` | upload | `b0a0f34ed1dfab65` |
| `lua/plugins/specs/lang/markdown.lua` | upload | `f12bd8029f0593e3` |
| `lua/plugins/specs/lang/python.lua` | upload | `6e74a24e2c239669` |
| `lua/plugins/specs/lang/rest.lua` | overlay batch4 (final version) | `d4ea9b9a62581952` |
| `lua/plugins/specs/lang/ruby.lua` | upload | `5b3095837f7e84dc` |
| `lua/plugins/specs/lang/rust.lua` | overlay batch5 (final version) | `84d7a5e618914a6b` |
| `lua/plugins/specs/lang/shared.lua` | upload | `4394b57c3e0aea7f` |
| `lua/plugins/specs/lang/typescript.lua` | upload | `947fb6e4ed09f577` |
| `lua/plugins/specs/lang/vhdl.lua` | upload | `dbb2f8e73e5fbdf4` |
| `lua/plugins/specs/lang/web.lua` | upload | `3326bfbd24f43646` |
| `lua/plugins/specs/lang/zig.lua` | upload | `5c2772377f958c43` |
| `lua/plugins/specs/lsp.lua` | overlay batch5 (final version) | `54ad4d2818ce42c8` |
| `lua/plugins/specs/test.lua` | upload | `b78c58c1623e830c` |
| `lua/plugins/specs/treesitter.lua` | overlay batch1 (final version) | `f30cb1c2461df238` |
| `lua/plugins/specs/ui.lua` | upload + terminal <Esc> fix (float on_open mapping removed; owned by keymaps.lua TermOpen) | `a018b60e48932190` |
| `lua/plugins/specs/workflow.lua` | upload | `9ec2eae15a380290` |
| `stylua.toml` | upload (root) | `99bae2ce80723316` |
| `scripts/gen_docs.lua` | upload (needed by tests/run.sh docs step) | `13cabe335320b469` |

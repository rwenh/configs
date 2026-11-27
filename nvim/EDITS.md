# Recalibration record

EDITS sections 1–9 were applied in batch 4, follow-up P0. Section 7's octo change was already present in the bundle.

- Section 1 (lazy.lua): applied, with one change. A failed registry load is fatal: `collect_specs` raises.
- Section 2 (gen_docs.lua): applied as written. gen_docs reads the registry only. `scripts/check_graph.lua` reports orphan specs.
- Sections 3–6: applied as written.
- Section 7 (setup calls): applied as written; octo was already applied.
- Section 8 (`.runner.lua`): applied as written.
- Section 9: applied. MANIFEST.md was regenerated.

This file is kept as the record. It is not a set of instructions to apply again.

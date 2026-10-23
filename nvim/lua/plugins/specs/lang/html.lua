-- lua/plugins/specs/lang/html.lua — HTML development
--

local shared = require("plugins.specs.lang.shared")

-- htmlhint's default rules are noisy, so it lints only projects that configure it.
shared.gate_linter({ "html", "htmldjango" }, "htmlhint", {
  files   = { ".htmlhintrc", ".htmlhint.json", ".htmlhint.js", "htmlhint.config.js" },
  pkg_key = "htmlhint",
  bin     = "htmlhint",
})

-- ── Snippets (queued; flushed once by completion.lua's LuaSnip config) ───────
require("core.util.snippets").register("html", function(s, t, i, _, ref)
  return {
    s("html5", {
      t({ "<!DOCTYPE html>", '<html lang="' }), i(1, "en"), t({ '">', "<head>",
        '  <meta charset="UTF-8" />',
        '  <meta name="viewport" content="width=device-width, initial-scale=1.0" />',
        "  <title>" }), i(2, "Document"), t({ "</title>", "</head>", "<body>", "  " }),
      i(0), t({ "", "</body>", "</html>" }),
    }),
    s("tag", {
      t("<"), i(1, "div"), t(' class="'), i(2), t('">'),
      t({ "", "  " }), i(0), t({ "", "</" }), ref(1, "div"), t(">"),
    }),
    s("inp", {
      t('<input type="'), i(1, "text"), t('" name="'), i(2),
      t('" id="'), ref(2), t('" placeholder="'), i(3), t('" />'),
    }),
  }
end)

return {
  shared.treesitter({ "html" }),
}

-- lua/plugins/specs/init.lua — plugin spec registry
--
-- Import ORDER IS LOAD-ORDER SENSITIVE for some entries:
--   completion  before lsp      (blink capabilities injected in lsp.lua)
--   lsp         before lang/*   (lang specs extend lsp servers/parsers)
--   web         before html/css (autotag + emmet consumed by html/css)
--   database    before sql      (dadbod owner; sql.lua is now a stub)
--
-- lang/shared.lua is a pure Lua module (no plugin specs); it is required
-- directly by lang specs at their module level — no import entry needed here.

return {
  -- ── Core plugin infrastructure ─────────────────────────────────────────────
  { import = "plugins.specs.ui"         },
  { import = "plugins.specs.editor"     },
  { import = "plugins.specs.completion" },   -- blink caps; must precede lsp
  { import = "plugins.specs.treesitter" },
  { import = "plugins.specs.lsp"        },   -- must precede lang/*
  { import = "plugins.specs.git"        },
  { import = "plugins.specs.dap"        },
  { import = "plugins.specs.test"       },
  { import = "plugins.specs.advanced"   },
  { import = "plugins.specs.hud"        },
  { import = "plugins.specs.workflow"   },

  -- ── Language specs ─────────────────────────────────────────────────────────
  -- Systems languages
  { import = "plugins.specs.lang.c"          },
  { import = "plugins.specs.lang.cpp"        },
  { import = "plugins.specs.lang.rust"       },
  { import = "plugins.specs.lang.go"         },
  { import = "plugins.specs.lang.python"     },
  { import = "plugins.specs.lang.java"       },
  { import = "plugins.specs.lang.kotlin"     },

  -- Web / scripted
  { import = "plugins.specs.lang.javascript" },
  { import = "plugins.specs.lang.typescript" },
  { import = "plugins.specs.lang.web"        },   -- autotag + emmet; BEFORE html/css
  { import = "plugins.specs.lang.html"       },
  { import = "plugins.specs.lang.css"        },

  -- Data / markup
  { import = "plugins.specs.lang.database"   },   -- dadbod owner; BEFORE sql
  { import = "plugins.specs.lang.sql"        },   -- stub; real config in database.lua
  { import = "plugins.specs.lang.markdown"   },
  { import = "plugins.specs.lang.rest"       },

  -- Scripted / JVM
  { import = "plugins.specs.lang.ruby"       },
  { import = "plugins.specs.lang.elixir"     },

  -- Esoteric / domain-specific
  { import = "plugins.specs.lang.fortran"    },
  { import = "plugins.specs.lang.zig"        },
  { import = "plugins.specs.lang.cobol"      },
  { import = "plugins.specs.lang.vhdl"       },
}

-- lua/plugins/specs/init.lua - Load all plugin specs

return {
  -- Core — ORDER MATTERS for dependencies:
  --   • completion must precede lsp  (blink capabilities injected in lsp.lua)
  --   • lsp must precede all lang/*  (lang specs add servers/parsers to lsp config)
  --   • web must precede html/css    (autotag + emmet setup consumed by html/css)
  --   • database must precede sql    (dadbod owner; sql defers to it)

  { import = "plugins.specs.ui" },
  { import = "plugins.specs.editor" },
  { import = "plugins.specs.completion" },   -- blink caps; must be before lsp
  { import = "plugins.specs.treesitter" },
  { import = "plugins.specs.lsp" },          -- must be before lang/*
  { import = "plugins.specs.git" },
  { import = "plugins.specs.dap" },
  { import = "plugins.specs.test" },
  { import = "plugins.specs.advanced" },
  { import = "plugins.specs.hud" },
  { import = "plugins.specs.workflow" },

  -- Languages (order preserved from original for stability)
  { import = "plugins.specs.lang.c" },
  { import = "plugins.specs.lang.cpp" },
  { import = "plugins.specs.lang.rust" },
  { import = "plugins.specs.lang.go" },
  { import = "plugins.specs.lang.python" },
  { import = "plugins.specs.lang.java" },
  { import = "plugins.specs.lang.kotlin" },
  { import = "plugins.specs.lang.javascript" },
  { import = "plugins.specs.lang.typescript" },
  { import = "plugins.specs.lang.web" },       -- autotag + emmet; before html/css
  { import = "plugins.specs.lang.html" },
  { import = "plugins.specs.lang.css" },
  { import = "plugins.specs.lang.database" },  -- dadbod owner; before sql
  { import = "plugins.specs.lang.sql" },
  { import = "plugins.specs.lang.markdown" },
  { import = "plugins.specs.lang.rest" },
  { import = "plugins.specs.lang.ruby" },
  { import = "plugins.specs.lang.elixir" },
  { import = "plugins.specs.lang.fortran" },
  { import = "plugins.specs.lang.zig" },
  { import = "plugins.specs.lang.cobol" },
  { import = "plugins.specs.lang.vhdl" },
}

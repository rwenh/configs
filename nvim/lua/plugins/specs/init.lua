-- lua/plugins/specs/init.lua - Load all plugin specs

return {
  -- Core — ORDER MATTERS for the following dependencies:
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

  -- Languages
  { import = "plugins.specs.lang.c" },
  { import = "plugins.specs.lang.cpp" },
  { import = "plugins.specs.lang.rust" },
  { import = "plugins.specs.lang.go" },
  { import = "plugins.specs.lang.python" },
  { import = "plugins.specs.lang.java" },
  { import = "plugins.specs.lang.kotlin" },
  { import = "plugins.specs.lang.javascript" },
  { import = "plugins.specs.lang.typescript" },
  { import = "plugins.specs.lang.web" },       -- autotag + emmet; must be before html/css
  { import = "plugins.specs.lang.html" },
  { import = "plugins.specs.lang.css" },
  { import = "plugins.specs.lang.database" },  -- dadbod owner; must be before sql
  { import = "plugins.specs.lang.sql" },
  { import = "plugins.specs.lang.markdown" },
  { import = "plugins.specs.lang.rest" },
  { import = "plugins.specs.lang.ruby" },
  { import = "plugins.specs.lang.elixir" },
  { import = "plugins.specs.lang.fortran" },
  { import = "plugins.specs.lang.zig" },
  { import = "plugins.specs.lang.cobol" },
  { import = "plugins.specs.lang.vhdl" },
  -- NOTE: Nim is intentionally omitted from lang/* specs. runner.lua has a
  -- nim runner but there is no LSP/treesitter/formatter spec for it yet.
  -- Add plugins.specs.lang.nim here if Nim support is needed.
}

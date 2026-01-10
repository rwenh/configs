-- lua/plugins/specs/init.lua - Load all plugin specs (updated with new languages)

return {
  -- UI
  { import = "plugins.specs.ui" },
  { import = "plugins.specs.editor" },
  { import = "plugins.specs.completion" },
  { import = "plugins.specs.treesitter" },
  { import = "plugins.specs.lsp" },
  { import = "plugins.specs.git" },
  { import = "plugins.specs.dap" },
  { import = "plugins.specs.test" },
  { import = "plugins.specs.advanced" },

  -- Languages
  { import = "plugins.specs.lang.rust" },
  { import = "plugins.specs.lang.python" },
  { import = "plugins.specs.lang.go" },
  { import = "plugins.specs.lang.c" },
  { import = "plugins.specs.lang.cpp" },
  { import = "plugins.specs.lang.fortran" },
  { import = "plugins.specs.lang.html" },
  { import = "plugins.specs.lang.javascript" },
  { import = "plugins.specs.lang.typescript" },
  { import = "plugins.specs.lang.java" },
  { import = "plugins.specs.lang.css" },
  { import = "plugins.specs.lang.sql" },
  { import = "plugins.specs.lang.web" },
  { import = "plugins.specs.lang.markdown" },
  { import = "plugins.specs.lang.database" },
  { import = "plugins.specs.lang.rest" },
  
  -- New languages
  { import = "plugins.specs.lang.ruby" },
  { import = "plugins.specs.lang.elixir" },
  { import = "plugins.specs.lang.kotlin" },
  { import = "plugins.specs.lang.zig" },
  { import = "plugins.specs.lang.cobol" },
  { import = "plugins.specs.lang.vhdl" },
}
-- lua/plugins/specs/lang/rust.lua - Rust development

return {
  {
    "mrcjkb/rustaceanvim",
    ft = "rust",
    opts = {},
  },

  {
    "saecki/crates.nvim",
    event = "BufRead Cargo. toml",
    opts = { completion = { cmp = { enabled = true } } },
  },
}
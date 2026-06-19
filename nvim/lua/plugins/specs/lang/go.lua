-- lua/plugins/specs/lang/go.lua — Go language support
--

local shared = require("plugins.specs.lang.shared")
local GO_FT = { "go", "gomod" }

local function check_go_env()
  local has_goimports = vim.fn.executable("goimports") == 1
  local has_gofumpt   = vim.fn.executable("gofumpt")   == 1
  if has_goimports and has_gofumpt then return end

  local gopath = vim.fn.trim(vim.fn.system("go env GOPATH 2>/dev/null"))
  if not gopath or gopath == "" then return end
  local bin_dir = gopath .. "/bin"
  local path    = os.getenv("PATH") or ""

  if not path:find(bin_dir, 1, true) then
    vim.notify(
      "[go] " .. bin_dir .. " is not in $PATH.\n"
      .. "goimports / gofumpt may not be found by formatters.\n"
      .. "Add to your shell profile: export PATH=$PATH:" .. bin_dir,
      vim.log.levels.WARN
    )
    return
  end

  if not has_goimports then
    vim.notify(
      "[go] goimports not found.\nInstall: go install golang.org/x/tools/cmd/goimports@latest",
      vim.log.levels.WARN
    )
  end
  if not has_gofumpt then
    vim.notify(
      "[go] gofumpt not found.\nInstall: go install mvdan.cc/gofumpt@latest",
      vim.log.levels.WARN
    )
  end
end

return {
  {
    "ray-x/go.nvim",
    dependencies = "ray-x/guihua.lua",
    ft    = GO_FT,
    build = false,
    config = function()
      local ok, err = pcall(function() require("go").setup({ lsp_cfg = false }) end)
      if not ok then vim.notify("go.nvim setup failed: " .. tostring(err), vim.log.levels.WARN); return end
      vim.schedule(check_go_env)
    end,
    keys = {
      { "<leader>got", "<cmd>GoTest<cr>",     desc = "Go Test",              ft = GO_FT },
      { "<leader>gof", "<cmd>GoTestFunc<cr>", desc = "Go Test Function",     ft = GO_FT },
      { "<leader>goc", "<cmd>GoCoverage<cr>", desc = "Go Coverage",          ft = GO_FT },
      { "<leader>gor", "<cmd>GoRun<cr>",      desc = "Go Run",               ft = GO_FT },
      { "<leader>gob", "<cmd>GoBuild<cr>",    desc = "Go Build",             ft = GO_FT },
      { "<leader>goi", "<cmd>GoImpl<cr>",     desc = "Go Impl",              ft = GO_FT },
      { "<leader>goa", "<cmd>GoAddTag<cr>",   desc = "Go Add Struct Tag",    ft = GO_FT },
      { "<leader>goA", "<cmd>GoRmTag<cr>",    desc = "Go Remove Struct Tag", ft = GO_FT },
      { "<leader>gom", "<cmd>GoMod<cr>",      desc = "Go Mod",               ft = GO_FT },
      { "<leader>gog", function() require("core.util.term").float_at_root("go generate ./...") end, desc = "Go Generate", ft = GO_FT },
      { "<leader>goe", function()
          local ok_t = pcall(function() require("core.util.term").float_at_root("go env") end)
          if not ok_t then vim.notify(vim.fn.system("go env 2>/dev/null"), vim.log.levels.INFO) end
        end, desc = "Go Env", ft = GO_FT },
      { "<leader>gov", function() require("core.util.term").float_at_root("go vet ./...") end, desc = "Go Vet", ft = GO_FT },
    },
  },
  shared.treesitter({ "go", "gomod", "gowork", "gosum" }),
}

-- lua/plugins/specs/lang/elixir.lua — Elixir development
--
-- Test mechanism guide:
--   <leader>ext   → mix test (project root)
--   <leader>exC   → mix test --cover (ExUnit coverage)
--   <leader>'n    → neotest-elixir (nearest ExUnit test)
--   <leader>'t    → runner.lua (mix test from project root)
--   Use neotest for interactive TDD; <leader>ext for quick full runs.
--

local shared = require("plugins.specs.lang.shared")

-- ── Elixir env guard ──────────────────────────────────────────────────────

local function check_elixir_env()
  if vim.fn.executable("mix") ~= 1 then
    vim.notify(
      "[elixir] `mix` not found — LSP and tests will not work.\n"
      .. "Install Elixir: https://elixir-lang.org/install.html",
      vim.log.levels.WARN
    )
    return
  end

  vim.system(
    { "mix", "hex.info" },
    { text = true },
    function(result)
      local output = (result.stdout or "") .. (result.stderr or "")
      if output:find("Could not find Hex", 1, true)
      or output:find("not available",      1, true)
      or result.code ~= 0 then
        vim.schedule(function()
          vim.notify(
            "[elixir] Hex package manager not installed.\n"
            .. "Run: mix local.hex --force",
            vim.log.levels.WARN
          )
        end)
      end
    end
  )
end

return {
  -- ── elixir-tools (NextLS) ─────────────────────────────────────────────────
  {
    "elixir-tools/elixir-tools.nvim",
    ft           = { "elixir", "eex", "heex", "surface" },
    dependencies = { "nvim-lua/plenary.nvim" },

    cond = function() return vim.g.elixir_use_nextls == true end,

    init = function()
      if vim.fn.executable("nextls") ~= 1 then
        vim.notify(
          "[elixir] vim.g.elixir_use_nextls = true but `nextls` binary not found.\n"
          .. "Download from: https://github.com/elixir-tools/next-ls/releases\n"
          .. "Place it somewhere on $PATH or set: vim.g.elixir_use_nextls = false\n"
          .. "to fall back to elixir-ls (managed by Mason).",
          vim.log.levels.WARN
        )
      end
    end,

    config = function()
      local ok, elixir = pcall(require, "elixir")
      if not ok then
        vim.notify("elixir-tools setup failed", vim.log.levels.WARN)
        return
      end
      local ok_setup = pcall(function()
        elixir.setup({
          elixirls = { enable = false },
          nextls   = {
            enable = true,
            cmd    = vim.g.nextls_bin or "nextls",
          },
        })
      end)
      if not ok_setup then
        vim.notify(
          "[elixir-tools] setup failed — check elixir-tools.nvim install.\n"
          .. "Try: :Lazy update elixir-tools.nvim",
          vim.log.levels.WARN
        )
      end
    end,
  },

  -- ── Keymaps ────────────────────────────────────────────────────────────────

  {
    "akinsho/toggleterm.nvim",
    ft   = "elixir",
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern  = "elixir",
        once     = true,
        group    = vim.api.nvim_create_augroup("ElixirEnvCheck", { clear = true }),
        -- check_elixir_env is now async internally; vim.schedule wrapper here
        -- is kept for consistency but is no longer load-critical.
        callback = function() vim.schedule(check_elixir_env) end,
      })
    end,
    keys = {
      {
        "<leader>ext",
        function() require("core.util.term").float_at_root("mix test") end,
        desc = "Elixir mix test",
        ft   = "elixir",
      },
      {
        "<leader>exC",
        function()
          require("core.util.term").float_at_root("mix test --cover")
        end,
        desc = "Elixir mix test --cover (ExUnit coverage)",
        ft   = "elixir",
      },
      {
        "<leader>exw",
        function()
          require("core.util.term").float_at_root(
            "mix test --listen-on-stdin --stale",
            { close_on_exit = false }
          )
        end,
        desc = "Elixir mix test --stale (watch mode)",
        ft   = "elixir",
      },
      {
        "<leader>exf",
        function() require("core.util.term").float_at_root("mix format") end,
        desc = "Elixir mix format",
        ft   = "elixir",
      },
      {
        "<leader>exp",
        function()
          require("core.util.term").float_at_root(
            "mix phx.server",
            { close_on_exit = false }
          )
        end,
        desc = "Elixir Phoenix server",
        ft   = "elixir",
      },
      {
        "<leader>exi",
        function() require("core.util.term").float_at_root("iex -S mix") end,
        desc = "Elixir IEx session",
        ft   = "elixir",
      },
      {
        "<leader>exd",
        function() require("core.util.term").float_at_root("mix deps.get") end,
        desc = "Elixir mix deps.get",
        ft   = "elixir",
      },
      {
        "<leader>exc",
        function() require("core.util.term").float_at_root("mix compile") end,
        desc = "Elixir mix compile",
        ft   = "elixir",
      },
    },
  },

  -- ── Treesitter ─────────────────────────────────────────────────────────────

  shared.treesitter({ "elixir", "heex", "eex" }),
}

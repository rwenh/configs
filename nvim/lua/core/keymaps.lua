-- lua/core/keymaps.lua — global keymaps
--

local map  = vim.keymap.set
local opts = { noremap = true, silent = true }

-- ── Lazy-require factory ──────────────────────────────────────────────────────
local function lazy(mod, tag)
  return function(fn)
    return function()
      local ok, m = pcall(require, mod)
      if ok then
        pcall(fn, m)
      else
        vim.notify(tag .. " not loaded", vim.log.levels.WARN)
      end
    end
  end
end

local dap_call     = lazy("dap",            "[dap] nvim-dap")
local dapui_call   = lazy("dapui",          "[dap] nvim-dap-ui")
local widget_call  = lazy("dap.ui.widgets", "[dap] nvim-dap")
local spectre_call = lazy("spectre",        "[spectre] not loaded — try :Lazy load nvim-spectre")
local harpoon_call = lazy("harpoon",        "[harpoon] not loaded — try :Lazy load harpoon")
local todo_call    = lazy("todo-comments",  "[todo-comments]")
local flash_call   = lazy("flash",          "[flash] not loaded — run :Lazy install and restart")
local focus_call   = lazy("core.focus",     "[focus]")

-- ── Basic editing ──────────────────────────────────────────────────────────────

map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)
map("n", "<A-j>", "<cmd>move .+1<cr>==", opts)
map("n", "<A-k>", "<cmd>move .-2<cr>==", opts)
map("x", "<A-j>", ":move '>+1<cr>gv=gv", opts)
map("x", "<A-k>", ":move '<-2<cr>gv=gv", opts)
map("n", "<Esc>", "<cmd>nohlsearch<cr>", opts)

-- ── Window management ─────────────────────────────────────────────────────────

map("n", "<leader>wq", function()
  local save_ok, save_err = pcall(vim.cmd, "write")
  if not save_ok then
    vim.notify(tostring(save_err):gsub("^.*E%d+: ", ""), vim.log.levels.WARN)
    return
  end
  pcall(vim.cmd, "quit")
end, { desc = "Save & Quit" })

map("n", "<leader>ww", function()
  local ok, err = pcall(vim.cmd, "write")
  if not ok then
    vim.notify(tostring(err):gsub("^.*E%d+: ", ""), vim.log.levels.WARN)
  end
end, { desc = "Save" })

map("n", "<leader>qq", "<cmd>q<cr>",  { desc = "Quit" })
map("n", "<leader>qa", "<cmd>qa<cr>", { desc = "Quit all" })

map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Vertical split"   })
map("n", "<leader>sh", "<cmd>split<cr>",  { desc = "Horizontal split" })
map("n", "<leader>se", "<C-w>=",          { desc = "Equal splits"     })
map("n", "<leader>sx", "<cmd>close<cr>",  { desc = "Close split"      })

map("n", "<leader>sm", function()
  local win = vim.api.nvim_get_current_win()
  local ok, maximized = pcall(vim.api.nvim_win_get_var, win, "_maximized")
  if ok and maximized then
    pcall(vim.api.nvim_win_del_var, win, "_maximized")
    vim.cmd("wincmd =")
  else
    pcall(vim.api.nvim_win_set_var, win, "_maximized", true)
    vim.cmd("wincmd | wincmd _")
  end
end, { desc = "Maximize / restore split" })

-- NOTE: <C-j>/<C-k> are normal-mode only; blink.cmp owns them in insert mode.
map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

map("n", "<C-Up>",    "<cmd>resize +2<cr>",          opts)
map("n", "<C-Down>",  "<cmd>resize -2<cr>",          opts)
map("n", "<C-Left>",  "<cmd>vertical resize -2<cr>", opts)
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", opts)

-- ── Buffer management ─────────────────────────────────────────────────────────

map("n", "<leader>bn", "<cmd>bnext<cr>",   { desc = "Next buffer"   })
map("n", "<leader>bp", "<cmd>bprev<cr>",   { desc = "Prev buffer"   })
map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
map("n", "<leader>bo", "<cmd>BufOnly<cr>", { desc = "Delete other buffers" })
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "[b", "<cmd>bprev<cr>", { desc = "Prev buffer" })

-- ── Telescope find ────────────────────────────────────────────────────────────

local telescope_maps = {
  { "<leader>ff", "find_files",   "Find files"         },
  { "<leader>fg", "git_files",    "Find git files"     },
  { "<leader>fw", "live_grep",    "Find word (grep)"   },
  { "<leader>fb", "buffers",      "Find buffers"       },
  { "<leader>fh", "help_tags",    "Find help"          },
  { "<leader>fm", "marks",        "Find marks"         },
  { "<leader>fk", "keymaps",      "Find keymaps"       },
  { "<leader>fc", "commands",     "Find commands"      },
  { "<leader>fo", "oldfiles",     "Recent files"       },
}

for _, spec in ipairs(telescope_maps) do
  local lhs, picker, desc = spec[1], spec[2], spec[3]
  map("n", lhs,
    function()
      local ok, tb = pcall(require, "telescope.builtin")
      if ok then pcall(tb[picker])
      else vim.notify("[telescope] not loaded", vim.log.levels.WARN) end
    end,
    { desc = desc })
end

-- Resume needs TelescopeResume (user command) not a builtin picker.
map("n", "<leader>fr", "<cmd>TelescopeResume<cr>", { desc = "Resume last search" })

map("n", "<C-s>", "<cmd>Telescope live_grep<cr>", { desc = "Live grep" })

-- ── Git ───────────────────────────────────────────────────────────────────────
-- NOTE: <leader>.p/.r/.S are buffer-local in git.lua (gitsigns on_attach).
--       <leader>.B is owned by blame.nvim keys= in git.lua.

map("n", "<leader>.g", "<cmd>LazyGit<cr>",                { desc = "LazyGit"       })
map("n", "<leader>.b", "<cmd>Telescope git_branches<cr>", { desc = "Git branches"  })
map("n", "<leader>.c", "<cmd>Telescope git_commits<cr>",  { desc = "Git commits"   })
map("n", "<leader>.s", "<cmd>Telescope git_status<cr>",   { desc = "Git status"    })
map("n", "<leader>.d", "<cmd>DiffviewOpen<cr>",           { desc = "Git diff"      })
map("n", "<leader>.h", "<cmd>DiffviewFileHistory<cr>",    { desc = "File history"  })

-- ── Debug (DAP) ───────────────────────────────────────────────────────────────

map("n", "<leader>;b", dap_call(function(d)  d.toggle_breakpoint() end), { desc = "Toggle breakpoint" })
map("n", "<leader>;c", dap_call(function(d)  d.continue()          end), { desc = "Continue/Start"    })
map("n", "<leader>;i", dap_call(function(d)  d.step_into()         end), { desc = "Step into"         })
map("n", "<leader>;o", dap_call(function(d)  d.step_over()         end), { desc = "Step over"         })
map("n", "<leader>;O", dap_call(function(d)  d.step_out()          end), { desc = "Step out"          })
map("n", "<leader>;L", dap_call(function(d)  d.run_last()          end), { desc = "Run last"          })
map("n", "<leader>;x", dap_call(function(d)  d.terminate()         end), { desc = "Terminate debug"   })
map("n", "<leader>;t", dapui_call(function(u) u.toggle()           end), { desc = "Toggle debug UI"   })
map("n", "<leader>;h", widget_call(function(w) w.hover()           end), { desc = "Debug hover"       })
map("n", "<leader>;p", widget_call(function(w) w.preview()         end), { desc = "Debug preview"     })

map("n", "<leader>;B", function()
  local ok, d = pcall(require, "dap")
  if ok then pcall(d.set_breakpoint, vim.fn.input("Breakpoint condition: "))
  else vim.notify("[dap] nvim-dap not loaded", vim.log.levels.WARN) end
end, { desc = "Conditional breakpoint" })

map("n", "<leader>;l", function()
  local ok, d = pcall(require, "dap")
  if ok then pcall(d.set_breakpoint, nil, nil, vim.fn.input("Log point message: "))
  else vim.notify("[dap] nvim-dap not loaded", vim.log.levels.WARN) end
end, { desc = "Log point" })

map("n", "<leader>;r", function()
  local ok, d = pcall(require, "dap")
  if ok then pcall(function() d.repl.toggle() end)
  else vim.notify("[dap] nvim-dap not loaded", vim.log.levels.WARN) end
end, { desc = "Toggle REPL" })

map("n", "<F5>",  dap_call(function(d) d.continue()          end), { desc = "Continue / Start"  })
map("n", "<F6>",  dap_call(function(d) d.toggle_breakpoint() end), { desc = "Toggle Breakpoint" })
map("n", "<F7>",  dap_call(function(d) d.step_into()         end), { desc = "Step Into"         })
map("n", "<F8>",  dap_call(function(d) d.step_over()         end), { desc = "Step Over"         })
map("n", "<F9>",  dap_call(function(d) d.step_out()          end), { desc = "Step Out"          })
map("n", "<F10>", dap_call(function(d) d.run_to_cursor()     end), { desc = "Run To Cursor"     })
map("n", "<F11>", dap_call(function(d) d.terminate()         end), { desc = "Terminate"         })

-- ── Run & test ────────────────────────────────────────────────────────────────

map("n", "<leader>'r", function()
  pcall(function() require("core.util.runner").run_file() end)
end, { desc = "Run file" })

map("x", "<leader>'s", function()
  local s = vim.fn.line("'<")
  local e = vim.fn.line("'>")
  if s == 0 or e == 0 then
    vim.notify("[runner] no visual selection found", vim.log.levels.WARN)
    return
  end
  pcall(function() require("core.util.runner").run_selection(s, e) end)
end, { desc = "Run selection" })

map("n", "<leader>'t", function()
  pcall(function() require("core.util.runner").run_tests() end)
end, { desc = "Run tests" })

-- ── Terminal ──────────────────────────────────────────────────────────────────
-- NOTE: <C-\> toggle is registered by toggleterm's own open_mapping = [[<C-\>]]
-- in ui.lua. Registering it here too would cause double-mapping.
-- The leader bindings below are supplementary controls not covered by open_mapping.

map("n", "<leader>\\t", "<cmd>ToggleTerm<cr>",                      { desc = "Terminal"           })
map("n", "<leader>\\f", "<cmd>ToggleTerm direction=float<cr>",      { desc = "Float terminal"     })
map("n", "<leader>\\h", "<cmd>ToggleTerm direction=horizontal<cr>", { desc = "Horizontal terminal"})
map("n", "<leader>\\v", "<cmd>ToggleTerm direction=vertical<cr>",   { desc = "Vertical terminal"  })
map("t", "<Esc>",  "<C-\\><C-n>", opts)

-- ── UI toggles ────────────────────────────────────────────────────────────────
-- NOTE: <leader>uz (ZenMode)  owned by specs/hud.lua zen-mode.nvim keys=.
--       <leader>uT (Twilight) owned by specs/hud.lua twilight.nvim keys=.

map("n", "<leader>ut", "<cmd>lua require('core.theme').toggle()<cr>", { desc = "Toggle theme"        })
map("n", "<leader>uw", "<cmd>ToggleWrap<cr>",                          { desc = "Toggle wrap"         })
map("n", "<leader>us", "<cmd>ToggleSpell<cr>",                         { desc = "Toggle spell"        })
map("n", "<leader>ul", "<cmd>set number! relativenumber!<cr>",         { desc = "Toggle line numbers" })

-- ── Search & replace ──────────────────────────────────────────────────────────

local _spectre_maps = {
  { "<leader>/s", function(s) s.open()                              end, "Search & replace"  },
  { "<leader>/w", function(s) s.open_visual({ select_word = true }) end, "Replace word"       },
  { "<leader>/f", function(s) s.open_file_search()                  end, "Replace in file"    },
}
for _, spec in ipairs(_spectre_maps) do
  map("n", spec[1], spectre_call(spec[2]), { desc = spec[3] })
end

-- ── Harpoon ───────────────────────────────────────────────────────────────────

map("n", "<leader>ha", harpoon_call(function(h) h:list():add() end),              { desc = "Harpoon add"  })
map("n", "<leader>hm", harpoon_call(function(h) h.ui:toggle_quick_menu(h:list()) end), { desc = "Harpoon menu" })
for i = 1, 4 do
  map("n", "<leader>h" .. i,
    harpoon_call(function(h) h:list():select(i) end),
    { desc = "Harpoon " .. i })
  map("n", "<M-" .. i .. ">",
    harpoon_call(function(h) h:list():select(i) end),
    opts)
end

-- ── Flash ─────────────────────────────────────────────────────────────────────

map({ "n", "x", "o" }, "s",
  flash_call(function(f) f.jump() end),
  { desc = "Flash jump" })

-- ── Misc utilities ────────────────────────────────────────────────────────────
-- NOTE: <leader>xx owned by ui.lua (Trouble keys=).
--       <leader>xu owned by advanced.lua (undotree keys=).
--       <leader>xg owned by advanced.lua (Neogen keys=).

map("n", "<leader>xc", "<cmd>CopyPath<cr>",    { desc = "Copy file path"       })
map("n", "<leader>xr", "<cmd>CopyRelPath<cr>", { desc = "Copy relative path"   })
map("n", "<leader>xd", "<cmd>cd %:p:h<cr>",    { desc = "Change to file dir"   })
map("n", "<leader>xe", "<cmd>!chmod +x %<cr>", { desc = "Make executable"      })
map("n", "<leader>xm", "<cmd>CleanUp<cr>",     { desc = "Clean memory"         })
map("n", "<leader>xh", "<cmd>Health<cr>",      { desc = "Health check"         })
map("n", "<leader>xp", "<cmd>ProjectRoot<cr>", { desc = "LCD to project root"  })
map("n", "<leader>xl", "<cmd>Lazy<cr>",        { desc = "Lazy"                 })
map("n", "<leader>xn", "<cmd>Mason<cr>",       { desc = "Mason"                })

-- ── TODO comments ─────────────────────────────────────────────────────────────

map("n", "]t", todo_call(function(tc) tc.jump_next() end), { desc = "Next todo"     })
map("n", "[t", todo_call(function(tc) tc.jump_prev() end), { desc = "Previous todo" })

-- ── Oil (convenience alias) ───────────────────────────────────────────────────

map("n", "-", "<cmd>Oil<cr>", { desc = "Open parent dir (Oil)" })

-- ── Noice ─────────────────────────────────────────────────────────────────────

map("n", "<leader>un", "<cmd>Noice dismiss<cr>", { desc = "Dismiss notifications"  })
map("n", "<leader>uN", "<cmd>Noice history<cr>", { desc = "Notification history"   })

-- ── Focus ─────────────────────────────────────────────────────────────────────

map("n", "<leader>uF",
  focus_call(function(f) f.toggle() end),
  { desc = "Deep focus mode" })

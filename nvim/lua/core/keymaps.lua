-- lua/core/keymaps.lua — global keymaps
--

local map  = vim.keymap.set
local opts = { noremap = true, silent = true }

-- ── Lazy-require factory ──────────────────────────────────────────────────────
local function lazy(mod, tag)
  return function(fn)
    return function()
      local ok, m = pcall(require, mod)
      if ok then pcall(fn, m)
      else vim.notify(tag .. " not loaded", vim.log.levels.WARN) end
    end
  end
end

local dap_call     = lazy("dap",            "[dap] nvim-dap")
local dapui_call   = lazy("dapui",          "[dap] nvim-dap-ui")
local widget_call  = lazy("dap.ui.widgets", "[dap] nvim-dap")
local spectre_call = lazy("spectre",        "[spectre] not loaded")
local harpoon_call = lazy("harpoon",        "[harpoon] not loaded")
local todo_call    = lazy("todo-comments",  "[todo-comments]")
local flash_call   = lazy("flash",          "[flash] not loaded")
local focus_call   = lazy("core.focus",     "[focus]")

-- ── Which-key group registry ──────────────────────────────────────────────────
--
-- Usage (from any lang spec or plugin config):
--   require("core.keymaps").register_group("<leader>kt", "kotlin")
--   require("core.keymaps").register_group("<leader>sp", "spring")

local _group_queue = {}

local function flush_groups()
  local ok, wk = pcall(require, "which-key")
  if not ok or #_group_queue == 0 then return end
  local specs = {}
  for _, g in ipairs(_group_queue) do
    table.insert(specs, { g.prefix, group = g.label })
  end
  pcall(function() wk.add(specs) end)
  _group_queue = {}
end

---@param prefix string   key prefix (e.g. "<leader>kt")
---@param label  string   group label shown in which-key (e.g. "kotlin")
function M_keymaps_register_group(prefix, label)
  table.insert(_group_queue, { prefix = prefix, label = label })
end

-- Expose on the module so callers can require("core.keymaps").register_group(...)
local M = {}
M.register_group = M_keymaps_register_group

vim.api.nvim_create_autocmd("User", {
  pattern  = "VeryLazy",
  once     = true,
  callback = flush_groups,
  desc     = "Flush which-key group registrations queued by lang specs",
})

-- ── :KeymapHealth ─────────────────────────────────────────────────────────────
--
-- Buffer-local maps are excluded.
vim.api.nvim_create_user_command("KeymapHealth", function()
  local maps = vim.api.nvim_get_keymap("n")
  local by_lhs = {}
  for _, m in ipairs(maps) do
    local lhs = m.lhs or ""
    by_lhs[lhs] = by_lhs[lhs] or {}
    table.insert(by_lhs[lhs], m.rhs or m.callback and "<callback>" or "?")
  end
  local conflicts = {}
  for lhs, rhs_list in pairs(by_lhs) do
    if #rhs_list > 1 then
      table.insert(conflicts, { lhs = lhs, rhs = rhs_list })
    end
  end
  if #conflicts == 0 then
    vim.notify("[keymaps] No conflicts found in global normal-mode keymaps.", vim.log.levels.INFO)
    return
  end
  table.sort(conflicts, function(a, b) return a.lhs < b.lhs end)
  local lines = { string.format("[keymaps] %d conflict(s):", #conflicts) }
  for _, c in ipairs(conflicts) do
    table.insert(lines, string.format("  %-20s → %s", c.lhs, table.concat(c.rhs, " | ")))
  end
  vim.notify(table.concat(lines, "\n"), vim.log.levels.WARN)
end, { desc = "Show duplicate global normal-mode keymaps" })

-- ── Basic editing ──────────────────────────────────────────────────────────────
map("v", "<",     "<gv",           { noremap=true, silent=true, desc="Indent left (keep selection)"  })
map("v", ">",     ">gv",           { noremap=true, silent=true, desc="Indent right (keep selection)" })
map("n", "<A-j>", "<cmd>move .+1<cr>==", { noremap=true, silent=true, desc="Move line down"  })
map("n", "<A-k>", "<cmd>move .-2<cr>==", { noremap=true, silent=true, desc="Move line up"    })
map("x", "<A-j>", ":move '>+1<cr>gv=gv", { noremap=true, silent=true, desc="Move selection down" })
map("x", "<A-k>", ":move '<-2<cr>gv=gv", { noremap=true, silent=true, desc="Move selection up"   })
map("n", "<Esc>", "<cmd>nohlsearch<cr>", { noremap=true, silent=true, desc="Clear search highlight" })

-- ── Window management ─────────────────────────────────────────────────────────
map("n", "<leader>wq", function()
  local save_ok, save_err = pcall(vim.cmd, "write")
  if not save_ok then vim.notify(tostring(save_err):gsub("^.*E%d+: ", ""), vim.log.levels.WARN); return end
  pcall(vim.cmd, "quit")
end, { desc = "Save & Quit" })

map("n", "<leader>ww", function()
  local ok, err = pcall(vim.cmd, "write")
  if not ok then vim.notify(tostring(err):gsub("^.*E%d+: ", ""), vim.log.levels.WARN) end
end, { desc = "Save" })

map("n", "<leader>qq", "<cmd>q<cr>",  { desc = "Quit"     })
map("n", "<leader>qa", "<cmd>qa<cr>", { desc = "Quit all" })
map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Vertical split"   })
map("n", "<leader>sh", "<cmd>split<cr>",  { desc = "Horizontal split" })
map("n", "<leader>se", "<C-w>=",          { desc = "Equal splits"     })
map("n", "<leader>sx", "<cmd>close<cr>",  { desc = "Close split"      })

map("n", "<leader>sm", function()
  local win = vim.api.nvim_get_current_win()
  local ok, maximized = pcall(vim.api.nvim_win_get_var, win, "_maximized")
  if ok and maximized then
    pcall(vim.api.nvim_win_del_var, win, "_maximized"); vim.cmd("wincmd =")
  else
    pcall(vim.api.nvim_win_set_var, win, "_maximized", true)
    vim.cmd("wincmd | wincmd _")
  end
end, { desc = "Maximize / restore split" })

map("n", "<C-h>", "<C-w>h", { noremap=true, silent=true, desc="Move to left split"  })
map("n", "<C-j>", "<C-w>j", { noremap=true, silent=true, desc="Move to lower split" })
map("n", "<C-k>", "<C-w>k", { noremap=true, silent=true, desc="Move to upper split" })
map("n", "<C-l>", "<C-w>l", { noremap=true, silent=true, desc="Move to right split" })
map("n", "<C-Up>",    "<cmd>resize +2<cr>",          { noremap=true, silent=true, desc="Increase split height" })
map("n", "<C-Down>",  "<cmd>resize -2<cr>",          { noremap=true, silent=true, desc="Decrease split height" })
map("n", "<C-Left>",  "<cmd>vertical resize -2<cr>", { noremap=true, silent=true, desc="Decrease split width"  })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { noremap=true, silent=true, desc="Increase split width"  })

-- ── Buffer management ─────────────────────────────────────────────────────────
map("n", "<leader>bn", "<cmd>bnext<cr>",   { desc = "Next buffer"   })
map("n", "<leader>bp", "<cmd>bprev<cr>",   { desc = "Prev buffer"   })
map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
map("n", "<leader>bo", "<cmd>BufOnly<cr>", { desc = "Delete other buffers" })
map("n", "]b", "<cmd>bnext<cr>", { noremap=true, silent=true, desc="Next buffer" })
map("n", "[b", "<cmd>bprev<cr>", { noremap=true, silent=true, desc="Prev buffer" })

-- ── Telescope ─────────────────────────────────────────────────────────────────
local telescope_maps = {
  { "<leader>ff", "find_files",  "Find files"       },
  { "<leader>fg", "git_files",   "Find git files"   },
  { "<leader>fw", "live_grep",   "Find word (grep)" },
  { "<leader>fb", "buffers",     "Find buffers"     },
  { "<leader>fh", "help_tags",   "Find help"        },
  { "<leader>fm", "marks",       "Find marks"       },
  { "<leader>fk", "keymaps",     "Find keymaps"     },
  { "<leader>fc", "commands",    "Find commands"    },
  { "<leader>fo", "oldfiles",    "Recent files"     },
}
for _, spec in ipairs(telescope_maps) do
  local lhs, picker, desc = spec[1], spec[2], spec[3]
  map("n", lhs, function()
    local ok, tb = pcall(require, "telescope.builtin")
    if ok then pcall(tb[picker]) else vim.notify("[telescope] not loaded", vim.log.levels.WARN) end
  end, { desc = desc })
end
map("n", "<leader>fr", "<cmd>TelescopeResume<cr>", { desc = "Resume last search" })
map("n", "<C-s>",      "<cmd>Telescope live_grep<cr>", { noremap=true, silent=true, desc="Live grep" })

-- ── Git ───────────────────────────────────────────────────────────────────────
map("n", "<leader>.g", "<cmd>LazyGit<cr>",                { desc = "LazyGit"      })
map("n", "<leader>.b", "<cmd>Telescope git_branches<cr>", { desc = "Git branches" })
map("n", "<leader>.c", "<cmd>Telescope git_commits<cr>",  { desc = "Git commits"  })
map("n", "<leader>.s", "<cmd>Telescope git_status<cr>",   { desc = "Git status"   })
map("n", "<leader>.d", "<cmd>DiffviewOpen<cr>",           { desc = "Git diff"     })
map("n", "<leader>.h", "<cmd>DiffviewFileHistory<cr>",    { desc = "File history" })

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
map("n", "<F5>",  dap_call(function(d) d.continue()          end), { desc = "DAP: Continue"          })
map("n", "<F6>",  dap_call(function(d) d.toggle_breakpoint() end), { desc = "DAP: Toggle Breakpoint" })
map("n", "<F7>",  dap_call(function(d) d.step_into()         end), { desc = "DAP: Step Into"         })
map("n", "<F8>",  dap_call(function(d) d.step_over()         end), { desc = "DAP: Step Over"         })
map("n", "<F9>",  dap_call(function(d) d.step_out()          end), { desc = "DAP: Step Out"          })
map("n", "<F10>", dap_call(function(d) d.run_to_cursor()     end), { desc = "DAP: Run To Cursor"     })
map("n", "<F11>", dap_call(function(d) d.terminate()         end), { desc = "DAP: Terminate"         })

-- ── Run & test ────────────────────────────────────────────────────────────────
map("n", "<leader>'r", function() pcall(function() require("core.util.runner").run_file() end) end,   { desc = "Run file"      })
map("n", "<leader>'t", function() pcall(function() require("core.util.runner").run_tests() end) end,  { desc = "Run tests"     })
map("x", "<leader>'s", function()
  local s = vim.fn.line("'<"); local e = vim.fn.line("'>")
  if s == 0 or e == 0 then vim.notify("[runner] no visual selection", vim.log.levels.WARN); return end
  pcall(function() require("core.util.runner").run_selection(s, e) end)
end, { desc = "Run selection" })
map("n", "<leader>'F", function()
  pcall(function() require("core.util.runner").run_nearest_function() end)
end, { desc = "Run nearest function (treesitter)" })

-- ── Terminal ──────────────────────────────────────────────────────────────────
map("n", "<leader>\\t", "<cmd>ToggleTerm<cr>",                      { desc = "Terminal"            })
map("n", "<leader>\\f", "<cmd>ToggleTerm direction=float<cr>",      { desc = "Float terminal"      })
map("n", "<leader>\\h", "<cmd>ToggleTerm direction=horizontal<cr>", { desc = "Horizontal terminal" })
map("n", "<leader>\\v", "<cmd>ToggleTerm direction=vertical<cr>",   { desc = "Vertical terminal"   })
map("t", "<Esc>", "<C-\\><C-n>", { noremap=true, silent=true, desc="Exit terminal mode" })

-- ── UI toggles ────────────────────────────────────────────────────────────────
map("n", "<leader>ut", "<cmd>lua require('core.theme').toggle()<cr>", { desc = "Toggle theme"        })
map("n", "<leader>uw", "<cmd>ToggleWrap<cr>",                          { desc = "Toggle wrap"         })
map("n", "<leader>us", "<cmd>ToggleSpell<cr>",                         { desc = "Toggle spell"        })
map("n", "<leader>ul", "<cmd>set number! relativenumber!<cr>",         { desc = "Toggle line numbers" })

-- ── Search & replace ──────────────────────────────────────────────────────────
for _, spec in ipairs({
  { "<leader>/s", function(s) s.open()                              end, "Search & replace (Spectre)" },
  { "<leader>/w", function(s) s.open_visual({ select_word = true }) end, "Replace word under cursor"  },
  { "<leader>/f", function(s) s.open_file_search()                  end, "Replace in current file"    },
}) do map("n", spec[1], spectre_call(spec[2]), { desc = spec[3] }) end

-- ── Harpoon ───────────────────────────────────────────────────────────────────
map("n", "<leader>ha", harpoon_call(function(h) h:list():add() end),                  { desc = "Harpoon add file" })
map("n", "<leader>hm", harpoon_call(function(h) h.ui:toggle_quick_menu(h:list()) end), { desc = "Harpoon menu"     })
for i = 1, 4 do
  map("n", "<leader>h" .. i, harpoon_call(function(h) h:list():select(i) end), { desc = "Harpoon jump " .. i })
  map("n", "<M-" .. i .. ">", harpoon_call(function(h) h:list():select(i) end), { noremap=true, silent=true, desc="Harpoon file " .. i })
end

-- ── Flash ─────────────────────────────────────────────────────────────────────
map({ "n","x","o" }, "s", flash_call(function(f) f.jump() end), { desc = "Flash jump" })

-- ── Utilities ─────────────────────────────────────────────────────────────────
map("n", "<leader>xc", "<cmd>CopyPath<cr>",    { desc = "Copy file path (absolute)" })
map("n", "<leader>xr", "<cmd>CopyRelPath<cr>", { desc = "Copy file path (relative)" })
map("n", "<leader>xd", "<cmd>cd %:p:h<cr>",    { desc = "CD to file directory"      })
map("n", "<leader>xe", "<cmd>!chmod +x %<cr>", { desc = "Make file executable"      })
map("n", "<leader>xm", "<cmd>CleanUp<cr>",     { desc = "Lua garbage collect"       })
map("n", "<leader>xh", "<cmd>Health<cr>",      { desc = "Health summary"            })
map("n", "<leader>xp", "<cmd>ProjectRoot<cr>", { desc = "LCD to project root"       })
map("n", "<leader>xl", "<cmd>Lazy<cr>",        { desc = "Lazy plugin manager"       })
map("n", "<leader>xn", "<cmd>Mason<cr>",       { desc = "Mason package manager"     })
map("n", "<leader>xK", "<cmd>KeymapHealth<cr>",{ desc = "Show keymap conflicts"     })

-- ── TODO comments ─────────────────────────────────────────────────────────────
map("n", "]t", todo_call(function(tc) tc.jump_next() end), { desc = "Next TODO"     })
map("n", "[t", todo_call(function(tc) tc.jump_prev() end), { desc = "Previous TODO" })

-- ── Oil ───────────────────────────────────────────────────────────────────────
map("n", "-", "<cmd>Oil<cr>", { desc = "Open parent directory (Oil)" })

-- ── Noice ─────────────────────────────────────────────────────────────────────
map("n", "<leader>un", "<cmd>Noice dismiss<cr>", { desc = "Dismiss notifications" })
map("n", "<leader>uN", "<cmd>Noice history<cr>", { desc = "Notification history"  })

-- ── Focus ─────────────────────────────────────────────────────────────────────
map("n", "<leader>uF", focus_call(function(f) f.toggle() end), { desc = "Deep focus mode" })

return M

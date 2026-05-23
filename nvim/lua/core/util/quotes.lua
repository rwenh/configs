-- lua/core/util/quotes.lua — curated quotes for the dashboard header
--
-- API:
--   M.random()           → random from all categories
--   M.random("craft")    → random from one category
--   M.weighted()         → time-of-day weighted selection (recommended)
--   M.session()          → one quote per session (stored in vim.g)
--   M.formatted(q)       → "text\n— author" display string
--

local M = {}

-- ── Quote table ───────────────────────────────────────────────────────────────

M.quotes = {

  -- ── CRAFT: code as communication, care, readability ──────────────────────

  {
    text     = "Any fool can write code that a computer can understand.\nGood programmers write code that humans can understand.",
    author   = "Martin Fowler",
    category = "craft",
  },
  {
    text     = "Programs must be written for people to read,\nand only incidentally for machines to execute.",
    author   = "Harold Abelson",
    category = "craft",
  },
  {
    text     = "Clean code always looks like it was written by someone who cares.",
    author   = "Robert C. Martin",
    category = "craft",
  },
  {
    text     = "The function of good software is to make the complex appear simple.",
    author   = "Grady Booch",
    category = "craft",
  },
  {
    text     = "Code is read much more often than it is written.",
    author   = "Guido van Rossum",
    category = "craft",
  },
  {
    text     = "Talk is cheap. Show me the code.",
    author   = "Linus Torvalds",
    category = "craft",
  },
  {
    text     = "First, solve the problem. Then, write the code.",
    author   = "anonymous",
    category = "craft",
  },

  -- ── DEBUG: the honest reality of what programming is ─────────────────────

  {
    text     = "Debugging is twice as hard as writing the code in the first place.\nTherefore, if you write the code as cleverly as possible,\nyou are, by definition, not smart enough to debug it.",
    author   = "Brian W. Kernighan",
    category = "debug",
  },
  {
    text     = "The best error message is the one that never shows up.",
    author   = "Thomas Fuchs",
    category = "debug",
  },
  {
    text     = "If debugging is the process of removing software bugs,\nthen programming must be the process of putting them in.",
    author   = "Edsger W. Dijkstra",
    category = "debug",
  },
  {
    text     = "The most effective debugging tool is still careful thought,\ncoupled with judiciously placed print statements.",
    author   = "Brian W. Kernighan",
    category = "debug",
  },
  {
    text     = "The first 90% of the code accounts for the first 90% of development time.\nThe remaining 10% of the code accounts for the other 90%.",
    author   = "Tom Cargill",
    category = "debug",
  },
  {
    text     = "Testing shows the presence, not the absence of bugs.",
    author   = "Edsger W. Dijkstra",
    category = "debug",
  },

  -- ── SIMPLICITY: less, restraint, elegance ────────────────────────────────

  {
    text     = "Simplicity is prerequisite for reliability.",
    author   = "Edsger W. Dijkstra",
    category = "simplicity",
  },
  {
    text     = "Perfection is achieved not when there is nothing more to add,\nbut when there is nothing left to take away.",
    author   = "Antoine de Saint-Exupéry",
    category = "simplicity",
  },
  {
    text     = "Premature optimization is the root of all evil.",
    author   = "Donald E. Knuth",
    category = "simplicity",
  },
  {
    text     = "The art of programming is the art of organizing complexity.",
    author   = "Edsger W. Dijkstra",
    category = "simplicity",
  },
  {
    text     = "There are two ways of constructing a software design:\nmake it so simple that there are obviously no deficiencies,\nor make it so complicated that there are no obvious deficiencies.",
    author   = "C.A.R. Hoare",
    category = "simplicity",
  },
  {
    text     = "The most important skill in software is knowing what to ignore.",
    author   = "anonymous",
    category = "simplicity",
  },

  -- ── GROWTH: failure, iteration, learning ─────────────────────────────────

  {
    text     = "The only way to learn a new programming language\nis by writing programs in it.",
    author   = "Dennis Ritchie",
    category = "growth",
  },
  {
    text     = "Make it work. Make it right. Make it fast.",
    author   = "Kent Beck",
    category = "growth",
  },
  {
    text     = "The best way to predict the future is to invent it.",
    author   = "Alan Kay",
    category = "growth",
  },
  {
    text     = "Every great developer you know got there by solving problems\nthey were unqualified to solve until they actually did it.",
    author   = "Patrick McKenzie",
    category = "growth",
  },
  {
    text     = "One of my most productive days was throwing away 1000 lines of code.",
    author   = "Ken Thompson",
    category = "growth",
  },
  {
    text     = "It always takes longer than you expect,\neven when you take into account Hofstadter's Law.",
    author   = "Douglas Hofstadter",
    category = "growth",
  },
  {
    text     = "In theory, theory and practice are the same.\nIn practice, they are not.",
    author   = "Albert Einstein",
    category = "growth",
  },

  -- ── HUMOR: dark, accurate, battle-tested ─────────────────────────────────

  {
    text     = "Always code as if the person who ends up maintaining your code\nwill be a violent psychopath who knows where you live.",
    author   = "John F. Woods",
    category = "humor",
  },
  {
    text     = "There are only two hard things in Computer Science:\ncache invalidation and naming things.",
    author   = "Phil Karlton",
    category = "humor",
  },
  {
    text     = "99 little bugs in the code.\n99 little bugs.\nTake one down, patch it around.\n127 little bugs in the code.",
    author   = "anonymous",
    category = "humor",
  },
  {
    text     = "It works on my machine.",
    author   = "every developer, 1992–present",
    category = "humor",
  },
  {
    text     = "A program that produces incorrect results twice as fast\nis infinitely slower.",
    author   = "John Osterhout",
    category = "humor",
  },
  {
    text     = "One bad programmer can easily create two new jobs a year.",
    author   = "David Parnas",
    category = "humor",
  },

  -- ── SYSTEMS: architecture, abstraction, scale ─────────────────────────────

  {
    text     = "All problems in computer science can be solved by another level of indirection.\nExcept for the problem of too many layers of indirection.",
    author   = "David Wheeler",
    category = "systems",
  },
  {
    text     = "Measuring programming progress by lines of code\nis like measuring aircraft building progress by weight.",
    author   = "Bill Gates",
    category = "systems",
  },
  {
    text     = "Software is like entropy: it is difficult to grasp, weighs nothing,\nand obeys the Second Law of Thermodynamics — it always increases.",
    author   = "Norman Augustine",
    category = "systems",
  },
  {
    text     = "Legacy code is simply code without tests.",
    author   = "Michael Feathers",
    category = "systems",
  },
  {
    text     = "The most disastrous thing you can ever learn\nis your first programming language.",
    author   = "Alan Kay",
    category = "systems",
  },
}

-- ── Weights ───────────────────────────────────────────────────────────────────
-- craft and debug are the most immediately relevant while coding.
-- They appear twice as often in weighted selection.
M._weights = {
  craft      = 2,
  debug      = 2,
  simplicity = 1,
  growth     = 1,
  humor      = 1,
  systems    = 1,
}

-- ── Helpers ───────────────────────────────────────────────────────────────────
-- Seed once at module load.  Calling math.randomseed() before every random()
-- call was redundant and counter-productive: multiple calls within the same
-- second share an identical seed, reducing effective randomness.
math.randomseed(os.time() + (vim.fn.getpid and vim.fn.getpid() or 0))

--- Return a random quote, optionally filtered by category.
---@param category string?
---@return table  { text, author, category }
function M.random(category)
  local pool = {}
  for _, q in ipairs(M.quotes) do
    if not category or q.category == category then
      table.insert(pool, q)
    end
  end
  if #pool == 0 then return M.quotes[1] end
  return pool[math.random(#pool)]
end

--- Weighted random selection with time-of-day override.
--
-- Time-of-day schedule:
--   06:00–08:59  → growth   (start the day with intention)
--   22:00+       → humor    (honest about late-night sessions)
--   all other    → category weights applied
--
---@return table  { text, author, category }
function M.weighted()
  local h = tonumber(os.date("%H")) or 12

  if h >= 6 and h < 9 then
    return M.random("growth")
  elseif h >= 22 then
    return M.random("humor")
  end

  local pool = {}
  for _, q in ipairs(M.quotes) do
    local w = M._weights[q.category] or 1
    for _ = 1, w do
      table.insert(pool, q)
    end
  end

  return pool[math.random(#pool)]
end

--- Return the session quote — one per Neovim session, chosen once at first
--- call and cached in vim.g so dashboard re-opens show the same quote.
---@return table  { text, author, category }
function M.session()
  if vim.g.nvim_session_quote_text and vim.g.nvim_session_quote_author then
    return {
      text     = vim.g.nvim_session_quote_text,
      author   = vim.g.nvim_session_quote_author,
      category = vim.g.nvim_session_quote_category or "craft",
    }
  end

  local q = M.weighted()
  -- vim.g cannot store tables; store fields separately
  vim.g.nvim_session_quote_text     = q.text
  vim.g.nvim_session_quote_author   = q.author
  vim.g.nvim_session_quote_category = q.category
  return q
end

--- Format a quote entry for display.
-- Anonymous and folk attributions are returned without an author line.
---@param  q table?  quote entry (defaults to M.session())
---@return string    display string, may contain \n
function M.formatted(q)
  q = q or M.session()

  local anon = not q.author
    or q.author == "anonymous"
    or q.author:find("every developer", 1, true)

  if anon then return q.text end
  return q.text .. "\n— " .. q.author
end

--- Return quote counts per category (for debugging / tests).
---@return table  { category = count }
function M.counts()
  local result = {}
  for _, q in ipairs(M.quotes) do
    result[q.category] = (result[q.category] or 0) + 1
  end
  return result
end

return M

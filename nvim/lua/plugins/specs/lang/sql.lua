-- lua/plugins/specs/lang/sql.lua — SQL stub
--
-- All SQL configuration has been consolidated into database.lua.
-- This file exists only as a thin re-export so existing references work.
--
-- NOTE: specs/init.lua import order requires database.lua before sql.lua.
-- Do not re-add formatter/parser specs here; they live in database.lua.

-- This file is intentionally empty.
-- The sqlfmt formatter, sql treesitter parser, and completion are all
-- registered in database.lua.
return {}

-- ~/.config/nvim/lua/utils/runner.lua

local M = {}
local helpers = require("utils.helpers")

-- Code runners for different languages
local code_runners = {
  python = function(file)
    local interpreters = { "python3", "python", "py" }
    for _, interpreter in ipairs(interpreters) do
      if helpers.command_exists(interpreter) then
        return interpreter .. " " .. vim.fn.shellescape(file)
      end
    end
    return nil
  end,

  rust = function(file)
    local dir = vim.fn.fnamemodify(file, ":h")
    if vim.fn.filereadable(helpers.join_path(dir, "Cargo.toml")) == 1 then
      return "cd " .. vim.fn.shellescape(dir) .. " && cargo run"
    else
      if not helpers.command_exists("rustc") then return nil end
      local exe = vim.fn.fnamemodify(file, ":r")
      return "rustc " .. vim.fn.shellescape(file) .. " -o " .. vim.fn.shellescape(exe) .. " && " .. vim.fn.shellescape(exe)
    end
  end,

  java = function(file)
    if not helpers.command_exists("javac") then return nil end
    local dir = vim.fn.fnamemodify(file, ":h")
    local name = vim.fn.fnamemodify(file, ":t:r")
    return "cd " .. vim.fn.shellescape(dir) .. " && javac " .. vim.fn.shellescape(file) .. " && java " .. name
  end,

  javascript = function(file)
    -- Check for Deno first
    if helpers.project_has_file("deno.json") or helpers.project_has_file("deno.jsonc") then
      if helpers.command_exists("deno") then
        return "deno run " .. vim.fn.shellescape(file)
      end
    end
    -- Fall back to Node.js
    if helpers.command_exists("node") then
      return "node " .. vim.fn.shellescape(file)
    end
    -- Try bun as last resort
    if helpers.command_exists("bun") then
      return "bun run " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  typescript = function(file)
    -- Check for Deno first
    if helpers.project_has_file("deno.json") or helpers.project_has_file("deno.jsonc") then
      if helpers.command_exists("deno") then
        return "deno run " .. vim.fn.shellescape(file)
      end
    end
    -- Try tsx (faster than ts-node)
    if helpers.command_exists("tsx") then
      return "tsx " .. vim.fn.shellescape(file)
    elseif helpers.command_exists("ts-node") then
      return "ts-node " .. vim.fn.shellescape(file)
    elseif helpers.command_exists("bun") then
      return "bun run " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  lua = function(file)
    if helpers.command_exists("lua") then
      return "lua " .. vim.fn.shellescape(file)
    elseif helpers.command_exists("luajit") then
      return "luajit " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  go = function(file)
    if not helpers.command_exists("go") then return nil end
    return "go run " .. vim.fn.shellescape(file)
  end,

  fortran = function(file)
    if not helpers.command_exists("gfortran") then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gfortran -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,

  c = function(file)
    if not helpers.command_exists("gcc") then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "gcc -Wall -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,

  cpp = function(file)
    if not helpers.command_exists("g++") then return nil end
    local exe = vim.fn.fnamemodify(file, ":r")
    return "g++ -Wall -std=c++17 -o " .. vim.fn.shellescape(exe) .. " " .. vim.fn.shellescape(file) .. " && " .. vim.fn.shellescape(exe)
  end,

  sql = function(file)
    if helpers.command_exists("psql") then
      return "psql -f " .. vim.fn.shellescape(file)
    elseif helpers.command_exists("mysql") then
      return "mysql < " .. vim.fn.shellescape(file)
    elseif helpers.command_exists("sqlite3") then
      return "sqlite3 < " .. vim.fn.shellescape(file)
    end
    return nil
  end,

  sh = function(file)
    return "bash " .. vim.fn.shellescape(file)
  end,

  bash = function(file)
    return "bash " .. vim.fn.shellescape(file)
  end,

  zsh = function(file)
    return "zsh " .. vim.fn.shellescape(file)
  end,
}

-- FIXED: Run current file with better error handling
function M.run_file()
  local file = vim.fn.expand("%:p")
  local filetype = vim.bo.filetype

  if file == "" or vim.fn.filereadable(file) ~= 1 then
    vim.notify("No valid file to run", vim.log.levels.ERROR)
    return
  end

  if vim.bo.modified then
    local choice = vim.fn.confirm("File has unsaved changes. Save before running?", "&Yes\n&No\n&Cancel", 1)
    if choice == 1 then
      vim.cmd("write")
    elseif choice == 3 then
      return
    end
  end

  local runner = code_runners[filetype]
  if not runner then
    vim.notify("No runner configured for filetype: " .. filetype, vim.log.levels.WARN)
    return
  end

  local cmd = runner(file)
  if not cmd then
    vim.notify("No suitable runtime found for " .. filetype, vim.log.levels.ERROR)
    return
  end

  local toggleterm_ok, Terminal = pcall(function()
    return require("toggleterm.terminal").Terminal
  end)

  if toggleterm_ok then
    local terminal = Terminal:new({
      cmd = cmd,
      direction = "float",
      close_on_exit = false,
      float_opts = { border = "curved" },
      -- FIXED: Add exit code handling
      on_exit = function(t, job, exit_code, name)
        if exit_code == 0 then
          vim.schedule(function()
            vim.notify("Process completed successfully", vim.log.levels.INFO)
          end)
        else
          vim.schedule(function()
            vim.notify(
              string.format("Process exited with code %d", exit_code),
              vim.log.levels.ERROR
            )
          end)
        end
      end,
    })
    terminal:toggle()
  else
    -- Fallback to built-in terminal
    vim.cmd("split | terminal " .. cmd)
    vim.cmd("startinsert")
  end
end

-- Build project with better detection
function M.build_project()
  local build_systems = {
    { file = "Makefile", cmd = "make", desc = "Make" },
    { file = "CMakeLists.txt", cmd = "cmake --build build", desc = "CMake" },
    { file = "Cargo.toml", cmd = "cargo build", desc = "Cargo" },
    { file = "build.gradle", cmd = "./gradlew build", desc = "Gradle" },
    { file = "build.gradle.kts", cmd = "./gradlew build", desc = "Gradle (Kotlin)" },
    { file = "pom.xml", cmd = "mvn package", desc = "Maven" },
    { file = "package.json", cmd = "npm run build", desc = "NPM" },
    { file = "go.mod", cmd = "go build", desc = "Go" },
    { file = "setup.py", cmd = "python setup.py build", desc = "Python setuptools" },
    { file = "pyproject.toml", cmd = "python -m build", desc = "Python build" },
  }

  local build_cmd = nil
  local build_desc = nil

  for _, system in ipairs(build_systems) do
    if vim.fn.filereadable(system.file) == 1 then
      build_cmd = system.cmd
      build_desc = system.desc
      break
    end
  end

  if not build_cmd then
    vim.notify("No known build system found in project", vim.log.levels.WARN)
    return
  end

  vim.notify("Building with " .. build_desc .. "...", vim.log.levels.INFO)

  local toggleterm_ok, Terminal = pcall(function()
    return require("toggleterm.terminal").Terminal
  end)

  if toggleterm_ok then
    local terminal = Terminal:new({
      cmd = build_cmd,
      direction = "horizontal",
      close_on_exit = false,
      -- FIXED: Add exit code handling
      on_exit = function(t, job, exit_code, name)
        if exit_code == 0 then
          vim.schedule(function()
            vim.notify("Build completed successfully", vim.log.levels.INFO)
          end)
        else
          vim.schedule(function()
            vim.notify(
              string.format("Build failed with exit code %d", exit_code),
              vim.log.levels.ERROR
            )
          end)
        end
      end,
    })
    terminal:toggle()
  else
    vim.cmd("split | terminal " .. build_cmd)
    vim.cmd("startinsert")
  end
end

return M

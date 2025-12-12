-- ~/.config/nvim/lua/utils/platform.lua
-- Platform-specific configuration (clipboard)

local helpers = require("utils.helpers")

if vim.fn.has("clipboard") == 0 then
  vim.schedule(function()
    vim.notify("Clipboard feature not available", vim.log.levels.WARN)
  end)
  return
end

local clipboard_ok = false

-- Wayland (wl-clipboard)
if vim.env.WAYLAND_DISPLAY and helpers.command_exists("wl-copy") then
  vim.g.clipboard = {
    name = "wayland",
    copy = {
      ["+"] = "wl-copy --type text/plain",
      ["*"] = "wl-copy --primary --type text/plain",
    },
    paste = {
      ["+"] = "wl-paste --no-newline",
      ["*"] = "wl-paste --primary --no-newline",
    },
  }
  clipboard_ok = true

-- X11 (xclip)
elseif vim.env.DISPLAY and helpers.command_exists("xclip") then
  vim.g.clipboard = {
    name = "xclip",
    copy = {
      ["+"] = "xclip -quiet -i -selection clipboard",
      ["*"] = "xclip -quiet -i -selection primary",
    },
    paste = {
      ["+"] = "xclip -o -selection clipboard",
      ["*"] = "xclip -o -selection primary",
    },
  }
  clipboard_ok = true

-- X11 (xsel)
elseif vim.env.DISPLAY and helpers.command_exists("xsel") then
  vim.g.clipboard = {
    name = "xsel",
    copy = {
      ["+"] = "xsel --clipboard --input",
      ["*"] = "xsel --primary --input",
    },
    paste = {
      ["+"] = "xsel --clipboard --output",
      ["*"] = "xsel --primary --output",
    },
  }
  clipboard_ok = true

-- WSL
elseif vim.fn.has("wsl") == 1 and helpers.command_exists("clip.exe") then
  vim.g.clipboard = {
    name = "WSL",
    copy = { ["+"] = "clip.exe", ["*"] = "clip.exe" },
    paste = {
      ["+"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
      ["*"] = 'powershell.exe -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))',
    },
  }
  clipboard_ok = true

-- macOS
elseif vim.fn.has("macunix") == 1 and helpers.command_exists("pbcopy") then
  vim.g.clipboard = {
    name = "macOS",
    copy = { ["+"] = "pbcopy", ["*"] = "pbcopy" },
    paste = { ["+"] = "pbpaste", ["*"] = "pbpaste" },
  }
  clipboard_ok = true

-- Windows (built-in)
elseif vim.fn.has("win32") == 1 then
  clipboard_ok = true  -- Windows has built-in clipboard support
end

-- Warn if no clipboard was configured
if not clipboard_ok then
  vim.schedule(function()
    vim.notify("No clipboard provider found", vim.log.levels.WARN)
  end)
end

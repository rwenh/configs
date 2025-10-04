# My openSUSE Leap 16.0 Dotfiles

Personal config files for Vim and Neovim. Tested on openSUSE Leap 16.0.

## Quick Install
Clone this repo and symlink manually:
```bash
git clone https://github.com/rwenh/configs.git ~/dotfiles
cd ~/dotfiles

# For Vim
ln -sf ~/dotfiles/.vimrc ~/.vimrc

# For Neovim (create dir if needed)
mkdir -p ~/.config/nvim
ln -sf ~/dotfiles/.init.lua ~/.config/nvim/init.lua

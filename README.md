# My openSUSE Leap 16.0 Dotfiles

Personal config files for Vim and Neovim. Tested on openSUSE Leap 16.0.

## Quick Install
Clone this repo and symlink manually:
git clone https://github.com/rwenh/configs.git ~/dotfiles
cd ~/dotfiles

# Vim (unchanged)
ln -sf ~/dotfiles/.vimrc ~/.vimrc

# Neovim – now points to the modular folder
ln -sf ~/dotfiles/nvim ~/.config/nvim

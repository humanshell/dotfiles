#!/bin/bash
#
# install.sh - this script installs and sets up links to the dotfiles in the user's home directory

# setup script variables
OS=$(uname)

# create soft links to config files and directories
ln -s $HOME/.dotfiles/rvmrc $HOME/.rvmrc
ln -s $HOME/.dotfiles/vim/vimrc $HOME/.vimrc
ln -s $HOME/.dotfiles/vim/ $HOME/.vim
ln -s $HOME/.dotfiles/htoprc $HOME/.htoprc

# create soft link to bash_config
if [ $OS == "Darwin" ]
  ln -s $HOME/.dotfiles/bash_config $HOME/.bash_profile
else
  ln -s $HOME/.dotfiles/bash_config $HOME/.bashrc
fi


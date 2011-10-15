#!/bin/bash
#
# install.sh - this script installs and sets up links to the dotfiles in the user's home directory

# setup script variables
OS=$(uname)

# create soft link to rvm config
ln -s $HOME/.dotfiles/rvmrc $HOME/.rvmrc

# create soft links to vim config files
ln -s $HOME/.dotfiles/vimrc $HOME/.vimrc

# create soft link to bash config
if [ $OS == "Darwin" ]
  ln -s $HOME/.dotfiles/bash_profile $HOME/.bash_profile
else
  ln -s $HOME/.dotfiles/bash_profile $HOME/.bashrc
fi


#!/bin/bash
#
# install.sh - this script installs and sets up links to the dotfiles in the user's home directory

# setup script variables
OS=$(uname)

# create soft links to config files
ln -s $HOME/.dotfiles/rvmrc $HOME/.rvmrc
ln -s $HOME/.dotfiles/htoprc $HOME/.htoprc
ln -s $HOME/.dotfiles/gitconfig $HOME/.gitconfig

# create soft link to bash_config
if [ $OS == "Darwin" ]; then
  ln -s $HOME/.dotfiles/bash_config $HOME/.bash_profile
else
  ln -s $HOME/.dotfiles/bash_config $HOME/.bashrc
fi

# install krisleech / vimfiles github repo
# https://github.com/krisleech/vimfiles
curl https://raw.github.com/krisleech/vimfiles/master/bootstrap.sh -o - | sh

# create the local vimrc overrides file
echo "set background=dark" > ~/.vimrc.local


#!/bin/bash
#
# install.sh
# - this script installs and sets up links to the dotfiles in the user's
# home directory and sets up the vim git submodule plugins

# setup script variables
OS=$(uname)
GIT=$(which git)

# create soft links to config files
ln -s $HOME/.dotfiles/rvmrc $HOME/.rvmrc
ln -s $HOME/.dotfiles/htoprc $HOME/.htoprc
ln -s $HOME/.dotfiles/gitconfig $HOME/.gitconfig
ln -s $HOME/.dotfiles/gemrc $HOME/.gemrc

# create soft link to bash_config
if [ $OS == "Darwin" ]; then
  ln -s $HOME/.dotfiles/bash_config $HOME/.bash_profile
else
  ln -s $HOME/.dotfiles/bash_config $HOME/.bashrc
fi

# initialize and update submodules
$GIT submodule init && $GIT submodule update


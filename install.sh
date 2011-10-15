#!/bin/bash
#
# install.sh - this script installs and sets up links to the dotfiles in the user's home directory

# setup script variables
OS=$(uname)

# create soft links to config files
ln -s $HOME/.dotfiles/rvmrc $HOME/.rvmrc

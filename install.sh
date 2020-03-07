#!/usr/bin/env bash
#
# install.sh
# - this script installs and sets up links to the dotfiles in the user's
# home directory and sets up the vim git submodule plugins

# setup script variables
OS=$(uname)
GIT=$(which git)

# create soft link to bash_config
if [[ $OS == "Darwin" || $OS == "FreeBSD" ]]; then
  [[ -f "$HOME/.bash_profile"   ]] && mv $HOME/.bash_profile $HOME/.bash_profile.bak
  [[ ! -h "$HOME/.bash_profile" ]] && ln -s $HOME/.dotfiles/bash_config $HOME/.bash_profile
else
  [[ -f "$HOME/.bashrc"   ]] && mv $HOME/.bashrc $HOME/.bashrc.bak
  [[ ! -h "$HOME/.bashrc" ]] && ln -s $HOME/.dotfiles/bash_config $HOME/.bashrc
fi

# create soft links to config files
[[ ! -h "$HOME/.htoprc"       ]] && ln -s $HOME/.dotfiles/htoprc $HOME/.htoprc
[[ ! -h "$HOME/.gitconfig"    ]] && ln -s $HOME/.dotfiles/gitconfig $HOME/.gitconfig
[[ ! -h "$HOME/.gemrc"        ]] && ln -s $HOME/.dotfiles/gemrc $HOME/.gemrc
[[ ! -h "$HOME/.rbenvrc"      ]] && ln -s $HOME/.dotfiles/rbenvrc $HOME/.rbenvrc
[[ ! -h "$HOME/.tmux.conf"    ]] && ln -s $HOME/.dotfiles/tmux.conf $HOME/.tmux.conf
[[ ! -h "$HOME/.vimrc.before" ]] && ln -s $HOME/.dotfiles/janus-vimrc-before $HOME/.vimrc.before
[[ ! -h "$HOME/.vimrc.after"  ]] && ln -s $HOME/.dotfiles/janus-vimrc-after $HOME/.vimrc.after

# create security soft links to /dev/null
[[ ! -h "$HOME/.mysql_history"  ]] && ln -s /dev/null $HOME/.mysql_history
[[ ! -h "$HOME/.sqlite_history" ]] && ln -s /dev/null $HOME/.sqlite_history
[[ ! -h "$HOME/.psql_history"   ]] && ln -s /dev/null $HOME/.psql_history
[[ ! -h "$HOME/.bash_history"   ]] && ln -s /dev/null $HOME/.bash_history

# customize some OSX features
if [[ $OS == "Darwin" ]]; then
  defaults write com.apple.dashboard mcx-disabled -bool true # disable dashboard
  hash tmutil &> /dev/null && sudo tmutil disablelocal # disable local time machine backups
fi

# install Janus
curl -Lo- https://bit.ly/janus-bootstrap | bash

# install additional vim plugins
[[ ! -d $HOME/.janus ]] && mkdir $HOME/.janus
[[ ! -d $HOME/.janus/vim-autoclose ]] && $GIT clone https://github.com/Townk/vim-autoclose.git $HOME/.janus/vim-autoclose

# install rbenv
if [[ ! -d $HOME/.rbenv ]]; then
  $GIT clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
  mkdir -p $HOME/.rbenv/plugins
  $GIT clone git://github.com/sstephenson/ruby-build.git $HOME/.rbenv/plugins/ruby-build
fi

#!/usr/bin/env bash
#
# install.sh
# - this script installs and sets up links to the dotfiles in the user's
# home directory and sets up the vim git submodule plugins

# setup script variables
OS=$(uname)
GIT=$(which git)

# create soft links to config files
[[ ! -h "$HOME/.htoprc" ]]   && ln -s $HOME/.dotfiles/htoprc $HOME/.htoprc
[[ ! -h "$HOME/.gtconfig" ]] && ln -s $HOME/.dotfiles/gitconfig $HOME/.gitconfig
[[ ! -h "$HOME/.gemrc" ]]    && ln -s $HOME/.dotfiles/gemrc $HOME/.gemrc
[[ ! -h "$HOME/.ackrc" ]]    && ln -s $HOME/.dotfiles/ackrc $HOME/.ackrc
[[ ! -h "$HOME/.vimrc" ]]    && ln -s $HOME/.dotfiles/vimrc $HOME/.vimrc
[[ ! -h "$HOME/.rbenvrc" ]]  && ln -s $HOME/.dotfiles/rbenvrc $HOME/.rbenvrc
[[ ! -h "$HOME/.phpenvrc" ]] && ln -s $HOME/.dotfiles/phpenvrc $HOME/.phpenvrc
[[ ! -h "$HOME/.vim" ]]      && ln -s $HOME/.dotfiles/vim $HOME/.vim

# create soft link to bash_config
if [[ $OS == "Darwin" || $OS == "FreeBSD" ]]; then
  [[ ! -h "$HOME/.bash_profile" ]] && ln -s $HOME/.dotfiles/bash_config $HOME/.bash_profile
else
  [[ ! -h "$HOME/.bashrc" ]] && ln -s $HOME/.dotfiles/bash_config $HOME/.bashrc
fi

# create soft links for mysql and sqlite histories to /dev/null
[[ ! -h "$HOME/.mysql_history" ]] && ln -s /dev/null $HOME/.mysql_history
[[ ! -h "$HOME/.sqlite_history" ]] && ln -s /dev/null $HOME/.sqlite_history

# grab pathogen.vim from github so submodule plugins work
if [[ $OS == "Darwin" ]]; then
  curl -so ~/.dotfiles/vim/autoload/pathogen.vim \
        https://raw.github.com/tpope/vim-pathogen/HEAD/autoload/pathogen.vim
else
  wget --no-check-certificate -O ~/.dotfiles/vim/autoload/pathogen.vim \
        https://raw.github.com/tpope/vim-pathogen/HEAD/autoload/pathogen.vim
fi

# initialize and update submodules
$GIT submodule init && $GIT submodule update

# install rbenv and ruby-build
if [[ ! -d $HOME/.rbenv ]]; then
  $GIT clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
  mkdir -p $HOME/.rbenv/plugins
  $GIT clone git://github.com/sstephenson/ruby-build.git $HOME/.rbenv/plugins/ruby-build
fi


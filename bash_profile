# Set personal path environment variables
export PATH=$PATH:.:~/Dropbox/bin

# load RVM
CC=gcc-4.2
[[ -s "/Users/humanshell/.rvm/scripts/rvm" ]] && source "/Users/humanshell/.rvm/scripts/rvm"

# Add TextMate as the default editor
export EDITOR=vim
export CVSEDITOR=vim
export SVN_EDITOR=vim

# Regular Colors
txtrst='\e[0m'          # Text Reset
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

# Bold Colors
BBlack='\e[1;30m'       # Black
BRed='\e[1;31m'         # Red
BGreen='\e[1;32m'       # Green
BYellow='\e[1;33m'      # Yellow
BBlue='\e[1;34m'        # Blue
BPurple='\e[1;35m'      # Purple
BCyan='\e[1;36m'        # Cyan
BWhite='\e[1;37m'       # White

# Setup a nice prompt (with git branch)
if [ -f /usr/local/git/contrib/completion/git-completion.bash ]; then
  . /usr/local/git/contrib/completion/git-completion.bash
fi

GIT_PS1_SHOWDIRTYSTATE=true

if [ -f /opt/local/etc/bash_completion ]; then
  . /opt/local/etc/bash_completion
fi

PS1="\n${BYellow}[${txtrst} \h-${BGreen}\u${txtrst} \w\$(__git_ps1) ${BYellow}]${txtrst}\n $ "

# Aliases to make the CLI a little easier to handle
alias cs="clear"
alias ls="ls -hG"
alias ll="ls -hlG"
alias la="ls -hlaG"
alias grep="grep --color=auto"
alias df="df -h"
alias du="du -shc"
alias top="htop"

# Aliases for git
alias gstat="git status"
alias gpush="git push origin master"
alias gpull="git pull origin master"
alias gdiff="git diff"

# This adds color to man pages
export PAGER="more"


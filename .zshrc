export ZSH=$HOME/.oh-my-zsh

export EDITOR=/usr/bin/nvim
export TERM=screen-256color

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku httpie nmap node npm nvm pip python rake ruby rvm ssh-agent systemd tmux tmuxinator vagrant virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

export NVM_DIR="/home/iszenasi/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

alias svndiff='svn diff | colordiff | less'
alias php5='docker run -it --rm -v "$PWD":/app/ -w /app php:5'

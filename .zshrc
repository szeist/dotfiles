export ZSH=$HOME/.oh-my-zsh
export EDITOR=/usr/bin/nvim

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku httpie nmap node npm nvm pip python rake ruby rvm ssh-agent systemd tmux tmuxinator vagrant virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

export NVM_DIR="$HOME/.nvm"
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.rvm/bin:$PATH:$GOPATH/bin
export WORKON_HOME=$HOME/.virtualenvs

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

[ -s ~/.rvm/scripts/rvm ] && source ~/.rvm/scripts/rvm

alias svndiff='svn diff | colordiff | less'
alias php5='docker run -it --rm -v "$PWD":/app/ -w /app php:5'
alias php7='docker run -it --rm -v "$PWD":/app/ -w /app php:7'
alias bfg='java -jar ~/opt/bfg.jar'
alias xclip='xclip -selection c'

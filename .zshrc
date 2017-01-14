export ZSH=$HOME/.oh-my-zsh

export EDITOR=/usr/bin/nvim
export TERM=xterm-256color

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku httpie nmap node npm nvm pip python rake ruby rvm ssh-agent systemd tmux tmuxinator vagrant virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

export NVM_DIR="/home/iszenasi/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

alias svndiff='svn diff | colordiff | less'
alias php5='docker run -it --rm -v "$PWD":/app/ -w /app php:5'

export ANDROID_HOME="/opt/android-sdk-linux/"
export PATH="${PATH}:${ANDROID_HOME}tools/:${ANDROID_HOME}platform-tools/"
export PATH=$PATH:/usr/local/go/bin


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export ZSH=$HOME/.oh-my-zsh

export EDITOR=/usr/bin/nvim
export TERM=xterm-256color

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bower bundler cabal capistrano composer django docker docker-compose gem git gulp heroku httpie nmap node npm nvm pip python rake ruby rvm ssh-agent systemd tmux tmuxinator vagrant virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

export NVM_DIR="/home/iszenasi/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

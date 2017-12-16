export ZSH=$HOME/.oh-my-zsh
export EDITOR=/usr/bin/nvim

export WORKON_HOME=~/.virtuanenvs
export GOPATH=~/.go
export PATH=$PATH:~/.go/bin:~/.gem/ruby/2.3.0/bin

# Predict magic
export PREDICT_SRC="$HOME/src/emarsys/predict"
export PYTHONPATH="$PREDICT_SRC"
export DROPBOX="$PREDICT_SRC/test-dropbox"
export DROPBOX_PROPERTIES="$DROPBOX/dropbox.properties"

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku nmap node npm nvm pip python rake ruby rvm systemd virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

alias xclip='xclip -selection c'

alias ssh="(ssh-add -l > /dev/null || ssh-add) && ssh"
alias scp="(ssh-add -l > /dev/null || ssh-add) && scp"
alias git="(ssh-add -l > /dev/null || ssh-add) && git"
alias code="(ssh-add -l > /dev/null || ssh-add) && code"

alias php5='docker run -it --rm -v "$PWD":/app/ -w /app php:5'
alias php7='docker run -it --rm -v "$PWD":/app/ -w /app php:7'

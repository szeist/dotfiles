export ZSH=$HOME/.oh-my-zsh
export EDITOR=/usr/bin/nvim
export TERM=xterm-256color

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku nmap node npm nvm pip python rake ruby rvm systemd tmux tmuxinator vagrant virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

alias svndiff='svn diff | colordiff | less'
alias bfg='java -jar ~/opt/bfg.jar'
alias xclip='xclip -selection c'

alias ssh="(ssh-add -l > /dev/null || ssh-add) && ssh"
alias scp="(ssh-add -l > /dev/null || ssh-add) && scp"
alias git="(ssh-add -l > /dev/null || ssh-add) && git"
alias code="(ssh-add -l > /dev/null || ssh-add) && code"

alias php5='docker run -it --rm -v "$PWD":/app/ -w /app php:5'
alias php7='docker run -it --rm -v "$PWD":/app/ -w /app php:7'

source $HOME/.profile
export ZSH=$HOME/.oh-my-zsh
export EDITOR=/usr/bin/nvim
export SRCDIR=$HOME/Personal/src

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku nmap node npm nvm pip python rake ruby rvm systemd task virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

[ -f $HOME/.zshrc.local ] && source $HOME/.zshrc.local

alias xclip='xclip -selection c'

alias ssh="(ssh-add -l > /dev/null || ssh-add) && ssh"
alias scp="(ssh-add -l > /dev/null || ssh-add) && scp"
alias git="(ssh-add -l > /dev/null || ssh-add) && git"
alias code="(ssh-add -l > /dev/null || ssh-add) && code"

alias php5='docker run -it --rm -v "$PWD":/app/ -w /app php:5'
alias php7='docker run -it --rm -v "$PWD":/app/ -w /app php:7'

alias say='spd-say -t female2 -l EN -r 5 -p -20'
alias wiki="nvim -c :VimwikiIndex"
alias tasksync="cd $SRCDIR/tasksync && pipenv run python todosync.py && cd - && task list project:home"
alias retro.sh="termdown 324 && say \"Security, wooooohooooooooo!\""

source $HOME/.profile
export ZSH=$HOME/.oh-my-zsh
export EDITOR=/usr/bin/nvim
export SRCDIR=$HOME/Personal/src

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(adb bundler cabal capistrano composer django docker docker-compose gem git grunt gulp heroku nmap node npm nvm pip python rake ruby rvm systemd task virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

[ -f $HOME/.zshrc.local ] && source $HOME/.zshrc.local

function jwt-decode() {
  sed -e 's/\./\n/g' <<< $(cut -d. -f1,2 <<< $1) | base64 -d | jq .
}

alias gpg='gpg2'

alias php5='docker run -it --rm --tty -v ${PWD}:/app -w /app php:5-cli php'
alias php7='docker run -it --rm --tty -v ${PWD}:/app -w /app php:7-cli php'
alias php='php7'

alias say='spd-say -t female2 -l EN -r 5 -p -20'

alias wiki='nvim -c :VimwikiIndex'

alias tasksync='cd $SRCDIR/tasksync && pipenv run python todosync.py && cd - && task list project:home'

alias composer='docker run --rm -i --tty -v ${PWD}:/app -v ${SSH_AUTH_SOCK}:/ssh-auth.sock -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro --user $(id -u):$(id -g) -e SSH_AUTH_SOCK=/ssh-auth.sock composer:latest'

alias xclip='xclip -selection c'

alias kali-on='VBoxManage startvm "Kali Linux" --type headless'
alias kali-off='VBoxManage controlvm "Kali Linux" poweroff'
alias kali-ssh='ssh root@$(VBoxManage guestproperty get "Kali Linux" /VirtualBox/GuestInfo/Net/1/V4/IP | cut -d" " -f2)'
alias kali-burp-proxy='http_proxy=http://$(VBoxManage guestproperty get "Kali Linux" /VirtualBox/GuestInfo/Net/1/V4/IP | cut -d" " -f2):8080 https_proxy=$http_proxy noproxy=""'

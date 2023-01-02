source $HOME/.profile
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(docker docker-compose git nmap taskwarrior)

source $ZSH/oh-my-zsh.sh

function bwunlock() {
  bw unlock --check || export BW_SESSION=$(bw unlock --raw)
  nohup bash -c 'sleep 300 && bw lock' > /dev/null 2>&1 &
}

function pvpn() {
  if [ "$1" = "s" ]; then
    protonvpn $@
    return $?
  fi
  sudo protonvpn $@
  py3-cmd refresh protonvpn
}

alias gpg='gpg2'

alias wiki='nvim -c :VimwikiIndex'

alias xclip='xclip -selection c'

alias kali='xhost local:root; docker run --rm -it --privileged -v $PWD:/app -v /tmp/.X11-unix:/tmp/.X11-unix -v $HOME/.Xauthority:/root/.Xauthority -e DISPLAY -w /app -v /dev/bus/usb:/dev/bus/usb -v /sys/fs/cgroup:/sys/fs/cgroup:ro --pid host --net host kali'

alias taw='task add project:work'
alias tah='task add project:home'
alias tal='task add project:home'
alias tlw='task minimal project:work'
alias tlh='task minimal project:home'
alias tll='task minimal project:learn'

alias recordscreen='ffmpeg -video_size 1920x1080 -framerate 12 -f x11grab -i :0.0'

[[ /usr/bin/kubectl ]] && source <(kubectl completion zsh)

[ -f $HOME/.zshrc.local ] && source $HOME/.zshrc.local

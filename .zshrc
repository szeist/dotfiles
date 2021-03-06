source $HOME/.profile
export ZSH=$HOME/.oh-my-zsh
export EDITOR=/usr/bin/nvim
export SRCDIR=$HOME/Personal/src

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(docker docker-compose git nmap task)

source $ZSH/oh-my-zsh.sh

[ -f $HOME/.zshrc.local ] && source $HOME/.zshrc.local

function jwt-decode() {
  sed -e 's/\./\n/g' <<< $(cut -d. -f1,2 <<< $1) | base64 -d | jq .
}

function ip2dec() {
  echo -n $1 | tr . "\n" | awk '{x = x * 256 + $1} END {print x}'
}

function bwunlock() {
  bw unlock --check || export BW_SESSION=$(bw unlock --raw)
  nohup bash -c 'sleep 300 && bw lock' > /dev/null 2>&1 &
}

alias gpg='gpg2'

alias wiki='nvim -c :VimwikiIndex'

alias xclip='xclip -selection c'

alias kali='xhost local:root; docker run --rm -it --privileged -v $PWD:/app -v /tmp/.X11-unix:/tmp/.X11-unix -v $HOME/.Xauthority:/root/.Xauthority -e DISPLAY -w /app -v /dev/bus/usb:/dev/bus/usb -v /sys/fs/cgroup:/sys/fs/cgroup:ro --pid host --net host kali'

alias audio-hdmi='pactl set-card-profile 0 output:hdmi-stereo-extra1'
alias audio-internal='pactl set-card-profile 0 output:analog-stereo'

alias taw='task add project:work due:today tag=pisti,blue'
alias tah='task add project:home due:today tag=pisti,blue'
alias tlw='task minimal project:work'
alias tlh='task minimal project:home'

alias recordscreen='ffmpeg -video_size 1920x1080 -framerate 12 -f x11grab -i :0.0'

###-tns-completion-start-###
if [ -f /home/iszenasi/.tnsrc ]; then 
    source /home/iszenasi/.tnsrc 
fi
###-tns-completion-end-###
[[ /usr/bin/kubectl ]] && source <(kubectl completion zsh)

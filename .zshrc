source $HOME/.profile
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="ys"
HIST_STAMPS="yyyy-mm-dd"

plugins=(docker docker-compose git nmap taskwarrior)

source $ZSH/oh-my-zsh.sh

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

alias kali='xhost local:root; docker run --rm -it --privileged -v $PWD:/app -v /tmp/.X11-unix:/tmp/.X11-unix -v $HOME/.Xauthority:/root/.Xauthority -e DISPLAY -w /app -v /dev/bus/usb:/dev/bus/usb -v /sys/fs/cgroup:/sys/fs/cgroup:ro --pid host --net host szeist-kali'

alias recordscreen='ffmpeg -video_size 1920x1080 -framerate 12 -f x11grab -i :0.0'

[[ /usr/bin/kubectl ]] && source <(kubectl completion zsh)

if [ -f "${HOME}/opt/todo.txt_cli/todo_completion" ]; then . "${HOME}/opt/todo.txt_cli/todo_completion"; fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f "${HOME}/opt/google-cloud-sdk/path.zsh.inc" ]; then . "${HOME}/opt/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "${HOME}/opt/google-cloud-sdk/completion.zsh.inc" ]; then . "${HOME}/opt/google-cloud-sdk/completion.zsh.inc"; fi

[ -f $HOME/.zshrc.local ] && source $HOME/.zshrc.local

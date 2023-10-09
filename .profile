export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

export WORKON_HOME=$HOME/.virtualenvs
export TERMINAL=xterm
export EDITOR=/usr/bin/nvim
export SRCDIR=$HOME/src

export PIPEWIRE_MODULE_DIR="/usr/lib64/pipewire-0.3/jack"
# For flutter
CHROME_EXECUTABLE=chromium

if [[ ":$PATH:" != *":$HOME/src/scripts:"* ]]; then
  PATH="$HOME/.local/bin:$HOME/src/scripts:$HOME/go/bin:$HOME/.npm-global/bin:$PATH:$HOME/opt/flutter/bin:$HOME/Android/Sdk/platform-tools"
  export PATH
fi

pgrep X > /dev/null
if [ $? -ne 0 ]; then
    startx;
fi

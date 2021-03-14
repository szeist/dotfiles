if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
fi

export SSH_AUTH_SOCK="/run/user/$(id -u)/gnupg/S.gpg-agent.ssh"

[ -f /usr/share/rvm/scripts/rvm ] && source /usr/share/rvm/scripts/rvm

export NVM_DIR="$HOME/.nvm"
export GOPATH=$HOME/.go
export WORKON_HOME=$HOME/.virtualenvs
export TERMINAL=xterm

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

[ -f /home/iszenasi/.travis/travis.sh ] && source /home/iszenasi/.travis/travis.sh

if [[ ":$PATH:" != *":$HOME/src/scripts:"* ]]; then
  PATH="$HOME/.local/bin:$HOME/src/scripts:$PATH"
  PATH="$GOPATH/bin:$PATH"
  export PATH
fi

export ANDROID_EMULATOR_USE_SYSTEM_LIBS=1
. /home/iszenasi/.profabevjava

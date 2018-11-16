if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
fi

export NVM_DIR="$HOME/.nvm"
export GOPATH=$HOME/.go
export WORKON_HOME=$HOME/.virtualenvs
export TERMINAL=xterm

if [[ ":$PATH:" != *":$HOME/src/scripts:"* ]]; then
  PATH="$HOME/.local/bin:$HOME/src/scripts:$PATH"
  PATH="$HOME/.cabal/bin:$PATH"
  PATH="$HOME/opt/android-studio/bin:$PATH"
  PATH="$HOME/.rvm/bin:$PATH"
  PATH="$HOME/.luarocks/bin:$PATH"
  PATH="$GOPATH/bin:$PATH"
  PATH="$HOME/opt/Android/Sdk/platform-tools:$PATH"
  export PATH
fi

export ANDROID_EMULATOR_USE_SYSTEM_LIBS=1

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

[ -f /home/iszenasi/.travis/travis.sh ] && source /home/iszenasi/.travis/travis.sh

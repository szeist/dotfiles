if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
fi

PATH="$HOME/.local/bin:$PATH:$HOME/src/scripts"
PATH="$HOME/.cabal/bin:$PATH"
PATH="$HOME/opt/android-studio/bin:$PATH"
export NVM_DIR="$HOME/.nvm"
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.rvm/bin:$PATH:$GOPATH/bin
export WORKON_HOME=$HOME/.virtualenvs
export TERMINAL=xterm


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

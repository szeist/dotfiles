export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

export WORKON_HOME=$HOME/.virtualenvs
export TERMINAL=xterm
export EDITOR=/usr/bin/nvim
export SRCDIR=$HOME/src

if [[ ":$PATH:" != *":$HOME/src/scripts:"* ]]; then
  PATH="$HOME/.local/bin:$HOME/src/scripts:$HOME/go/bin:$PATH"
  export PATH
fi

pgrep X > /dev/null
if [ $? -ne 0 ]; then
    startx;
fi

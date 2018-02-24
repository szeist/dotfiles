# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
PATH="$HOME/opt/android-studio/bin:$PATH"
export NVM_DIR="$HOME/.nvm"
export GOPATH=$HOME/.go
export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/.rvm/bin:$PATH:$GOPATH/bin
export WORKON_HOME=$HOME/.virtualenvs
export TERMINAL=xterm


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

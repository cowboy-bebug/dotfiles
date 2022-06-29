# XDG Base Directory Specification
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export PATH="$HOME/.local/bin:$PATH"
export CLICOLOR=1

export LESSHISTFILE="-"
export SHELL_SESSIONS_DISABLE=1

export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"

export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

alias yarn='yarn --use-yarnrc "$XDG_CONFIG_HOME/yarn/config"'

export AZURE_CONFIG_DIR="$XDG_DATA_HOME/azure"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPATH="$XDG_DATA_HOME/go"
# export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export IPFS_PATH="$XDG_DATA_HOME/ipfs"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export TERMINFO="$XDG_DATA_HOME/terminfo"
export TERMINFO_DIRS="$XDG_DATA_HOME/terminfo:/usr/share/terminfo"

export PATH="$CARGO_HOME/bin:$PATH"
. "/home/eric/.local/share/cargo/env"

# ~/.zshenv should source this file:
# echo "source ~/.config/zsh/.zshenv" >> ~/.zshenv

# XDG Base Directory Specification
export XDG_BIN_HOME="$HOME/.local/bin"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# XDG_CACHE_HOME
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME/bundle"
export CP_HOME_DIR="$XDG_CACHE_HOME/cocoapod"

# XDG_CONFIG_HOME
export BAT_CONFIG_PATH="$XDG_CONFIG_HOME/bat/bat.conf"
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME/bundle"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# XDG_DATA_HOME
export AZURE_CONFIG_DIR="$XDG_DATA_HOME/azure"
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME/bundle"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GEM_HOME="$XDG_DATA_HOME/gem"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$XDG_DATA_HOME/go"
export IPFS_PATH="$XDG_DATA_HOME/ipfs"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
export RBENV_ROOT="$XDG_DATA_HOME/rbenv"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export TERMINFO="$XDG_DATA_HOME/terminfo"
export TERMINFO_DIRS="$XDG_DATA_HOME/terminfo:/usr/share/terminfo"

# miscellaneous
export ANDROID_HOME=$HOME/Android/Sdk
export DOTNET_ROOT=$HOME/.dotnet
export HOMEBREW_BUNDLE_DUMP_NO_VSCODE=1
export HOMEBREW_NO_AUTO_UPDATE=1
export PYENV_ROOT="$XDG_DATA_HOME/pyenv"

# paths
export PATH="$PATH:$ANDROID_SDK_ROOT/tools:$ANDROID_SDK_ROOT/platform-tools"
export PATH="$PATH:$CARGO_HOME/bin"
export PATH="$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$PNPM_HOME"
export PATH="$PATH:$XDG_BIN_HOME"
export PATH="$PATH:$XDG_CONFIG_HOME/emacs/bin"
export PATH="$PATH:/opt/homebrew/bin:/usr/local/bin"
export PATH="$PATH:$PYENV_ROOT/bin"

# Other exports
export DOCKER_HOST=unix:///Users/$USER/.docker/run/docker.sock
export LESSHISTFILE="-"

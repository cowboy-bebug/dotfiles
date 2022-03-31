# aliases
alias {v,vi,vim}="nvim"
alias {k,kube}="kubectl"

# editor
export VISUAL="nvim"
export EDITOR=$VISUAL

# git
export GPG_TTY=$(tty)
local NAME="Eric"
local EMAIL="cowboy-bebug@users.noreply.github.com"
local SIGNING_KEY=$(gpg -K --keyid-format=long --with-colons $EMAIL | grep ^sec:u | cut -d":" -f5)
git config --global core.pager "less -F -X"
git config --global commit.gpgsign true
git config --global diff.wsErrorHighlight "all"
git config --global format.pretty "oneline"
git config --global gpg.program "/usr/local/bin/gpg"
git config --global log.abbrevCommit true
git config --global pull.rebase true
git config --global user.name $NAME
git config --global user.email $EMAIL
git config --global user.signingkey $SIGNING_KEY

# homebrew
export HOMEBREW_BUNDLE_FILE=$HOME/.config/brew/Brewfile
export PATH="/usr/local/sbin:$PATH"

# zsh
local ZSH_CACHE="$HOME/.cache/zsh" && mkdir -p $ZSH_CACHE
export HISTFILE="$ZSH_CACHE/.zsh_history"

# oh-my-zsh
export ZSH_COMPDUMP="$HOME/.cache/zsh/.zcompdump-$HOST-$ZSH_VERSION"
export ZSH="$HOME/.oh-my-zsh"
zstyle ":omz:update" mode auto
zstyle ":omz:update" frequecy 7
plugins=(git)
source $ZSH/oh-my-zsh.sh

# starship
eval "$(starship init zsh)"

# XDG Base Directory Specification
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# extra
export PATH="$PATH:$(go env GOPATH)/bin"

bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word

# nvm
export NVM_DIR=$HOME/.nvm
if [ -f $(brew --prefix)/nvm.sh ]; then
  source $(brew --prefix)/nvm.sh
fi
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# aliases
alias {v,vi,vim}="nvim"
alias {k,kube}="kubectl"

# editor
export VISUAL="nvim"
export EDITOR=$VISUAL

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

# extra
export PATH="$PATH:$(go env GOPATH)/bin"
export GPG_TTY=$(tty)

bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word

# nvm
export NVM_DIR=$HOME/.nvm
if [ -f $(brew --prefix)/nvm.sh ]; then
  source $(brew --prefix)/nvm.sh
fi
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$PATH:$(go env GOPATH)/bin"
export GPG_TTY=$(tty)
export HOMEBREW_BUNDLE_FILE=$HOME/.config/brew/Brewfile
export ZSH=$HOME/.oh-my-zsh

bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word

# nvm
export NVM_DIR=$HOME/.nvm
if [ -f $(brew --prefix)/nvm.sh ]; then
  source $(brew --prefix)/nvm.sh
fi

eval "$(starship init zsh)"

source $ZSH/oh-my-zsh.sh
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

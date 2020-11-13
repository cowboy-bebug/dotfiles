export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$HOME/bin:/usr/local/sbin:$PATH

export GPG_TTY=$(tty)
export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/.oh-my-zsh-custom

bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word

ZSH_THEME="engreek"
HIST_STAMPS="yyyy-mm-dd"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# brew-file
if [ -f $(brew --prefix)/etc/brew-wrap ]; then
  source $(brew --prefix)/etc/brew-wrap
fi

# nvm
export NVM_DIR=$HOME/.nvm
if [ -f $(brew --prefix)/nvm.sh ]; then
  source $(brew --prefix)/nvm.sh
fi

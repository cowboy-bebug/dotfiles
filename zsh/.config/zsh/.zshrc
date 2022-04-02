# colours
autoload -U colors && colors

# history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE="$XDG_CACHE_HOME/zsh/history"

# auto / tab complete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # hidden files

# vim mode
bindkey -v
export KEYTIMEOUT=1

# change cursor shape for different vi modes
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select

# use beam shape cursor on startup
echo -ne '\e[5 q'

# use beam shape cursor for each new prompt
precmd() { echo -ne '\e[5 q'; }

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

# key binds
bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word
bindkey "^R" history-incremental-search-backward

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform

source "$XDG_CONFIG_HOME/fzf/fzf.zsh"
source "$(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
eval "$(starship init zsh)"

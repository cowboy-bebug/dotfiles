# colours
autoload -U colors && colors

# options
setopt INTERACTIVE_COMMENTS
setopt SHARE_HISTORY

# history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE_DIR="$XDG_CACHE_HOME/zsh"
HISTFILE="$HISTFILE_DIR/history"
[[ -f $HISTFILE ]] || mkdir -p $HISTFILE_DIR && touch $HISTFILE

# auto / tab complete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots) # hidden files

# aliases
alias {v,vi,vim}="nvim"
alias ls="ls --color=auto -v"

# editor
export VISUAL="nvim"
export EDITOR=$VISUAL

# git
export GPG_TTY=$(tty)

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform

# load env
source "$CARGO_HOME/env"

# ZSH plugins
ZSH_PLUGIN_DIR="$XDG_DATA_HOME/zsh"
ZSH_PLUGINS=(
  "$ZSH_PLUGIN_DIR/junegunn/fzf/fzf.zsh"
  "$ZSH_PLUGIN_DIR/Aloxaf/fzf-tab/fzf-tab.plugin.zsh"
  "$ZSH_PLUGIN_DIR/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
  "$ZSH_PLUGIN_DIR/jeffreytse/zsh-vi-mode/zsh-vi-mode.plugin.zsh"
)
for plugin in $ZSH_PLUGINS; do
  [ -f $plugin ] && source $plugin
done

[ -x "$(command -v starship)" ] && eval "$(starship init zsh)"

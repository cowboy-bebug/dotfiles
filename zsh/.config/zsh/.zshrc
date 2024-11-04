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
alias diff="diff --color -y"
alias {v,vi,vim}="nvim"
alias ls="ls --color=auto -v"
alias wget="wget --hsts-file=$XDG_CACHE_HOME/wget-hsts"

# browser
export BROWSER="brave"

# editor
export VISUAL="nvim"
export EDITOR=$VISUAL

# git
export GPG_TTY=$(tty)

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform

# load env
[ -e "$CARGO_HOME/env" ] && source "$CARGO_HOME/env"

# ZSH plugins
ZSH_PLUGIN_DIR="$XDG_DATA_HOME/zsh"
ZSH_PLUGINS=(
  "$ZSH_PLUGIN_DIR/fzf-tab/fzf-tab.plugin.zsh"
  "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
  "$ZSH_PLUGIN_DIR/zsh-vi-mode/zsh-vi-mode.plugin.zsh"
)
for plugin in $ZSH_PLUGINS; do
  [ -f $plugin ] && source $plugin
done

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -x "$(command -v starship)" ] && eval "$(starship init zsh)"
[ -x "$(command -v fastfetch)" ] && fastfetch
[ -x "$(command fzf --version)" ] && eval "source <(fzf --zsh)"

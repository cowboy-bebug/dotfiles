# colours
autoload -U colors && colors

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
local GIT_CONFIG_DIR=$XDG_CONFIG_HOME/git
local GIT_CONFIG=$GIT_CONFIG_DIR/config
[[ -f $GIT_CONFIG ]] || mkdir -p $GIT_CONFIG_DIR && touch $GIT_CONFIG
export GPG_TTY=$(tty)
local NAME="Eric"
local EMAIL="cowboy-bebug@users.noreply.github.com"
local SIGNING_KEY=$(gpg -K --keyid-format=long --with-colons $EMAIL | grep ^sec:u | cut -d":" -f5)
git config --global core.pager "less -F -X"
git config --global commit.gpgsign true
git config --global diff.wsErrorHighlight "all"
git config --global format.pretty "oneline"
git config --global gpg.program "/usr/bin/gpg"
git config --global log.abbrevCommit true
git config --global pull.rebase true
git config --global push.autoSetupRemote true
git config --global remote.upstream.tagOpt --no-tags
git config --global user.name $NAME
git config --global user.email $EMAIL
git config --global user.signingkey $SIGNING_KEY

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/local/bin/terraform terraform

# ZSH plugins
ZSH_PLUGIN_DIR="$XDG_DATA_HOME/zsh"
ZSH_PLUGINS=(
  "$ZSH_PLUGIN_DIR/fzf/fzf.zsh"
  "$ZSH_PLUGIN_DIR/fzf-tab/fzf-tab.plugin.zsh"
  "$ZSH_PLUGIN_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
  "$ZSH_PLUGIN_DIR/zsh-vi-mode/zsh-vi-mode.plugin.zsh"
)
for plugin in $ZSH_PLUGINS; do
  [ -f $plugin ] && source $plugin
done

[ -x "$(command -v starship)" ] && eval "$(starship init zsh)"

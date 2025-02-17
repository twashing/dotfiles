#!/usr/bin/env zsh

# # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# # Initialization code that may require console input (password prompts, [y/n]
# # confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

# # Add in Powerlevel10k
# zinit ice depth=1; zinit light romkatv/powerlevel10k

# Add in zsh plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

# Add in snippets
zinit snippet OMZL::git.zsh
zinit snippet OMZP::git
zinit snippet OMZP::sudo    # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/sudo
# zinit snippet OMZP::aws
zinit snippet OMZP::kubectl
zinit snippet OMZP::kubectx
zinit snippet OMZP::command-not-found
zinit snippet OMZP::nmap    # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/nmap
zinit snippet OMZP::ngrok   # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/ngrok
zinit snippet OMZP::rclone  # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/rclone
zinit snippet OMZP::rsync   # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/rsync
zinit snippet OMZP::ssh     # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/ssh
zinit snippet OMZP::tldr    # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tldr


# Load completions
autoload -Uz compinit && compinit

zinit cdreplay -q

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Keybindings
bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey '^[w' kill-region

# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'


# #######
# PATH
# #######

if [ -d "$HOME/.local/share" ]; then
  PATH="$HOME/.local/share:$PATH"
fi

if [ -d "$HOME/.local/bin" ]; then
  PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.bin" ]; then
  PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/bin" ]; then
  PATH="$HOME/bin:$PATH"
fi


# #######
# Navigation
# #######
set -o emacs


# #######
# Turn on extended glob option
# #######
setopt extended_glob



# #######
# Functions
# #######

function git_log_tree {
  git log --all --graph --decorate --oneline --simplify-by-decoration
}

function find_in_jar {
  find $1 -name "*.jar" -exec sh -c "echo; echo Inspecting: {}; jar tvf {} | grep $2" \;
}

# taken from: https://twitter.com/_komaz/status/514628905576460288
function mkcd {
    mkdir -p "$1" && cd "$1"
}

# taken from: http://cfenollosa.com/misc/tricks.txt
function psgrep { ps axuf | grep -v grep | grep "$@" -i --color=auto; }

function fname { find . -iname "*$@*"; }


# Docker configuration
# eval $(docker-machine env default)
function docker_remove_untagged_images {
  docker rmi -f $(docker images | grep "^<none>" | awk '{print $3}')
}

function docker_clear_containers {
  docker rm -f $(docker ps -a -q)
}

function docker_clear_images {
  docker rmi -f $(docker images -a -q)
}

function docker_clear_volumes {
  docker volume rm $(docker volume ls -q)
}

function docker_clear_networks {
  docker network rm $(docker network ls | tail -n+2 | awk '{if($2 !~ /bridge|none|host/){ print $1 }}')
}

function docker_attach_to_container {
  docker exec -i -t $1 /bin/bash
}

function docker_container_ip_address {
  docker inspect $1
}

function git_pull_all {
  for i in `ls`; do echo "Pulling: $i"; cd $i; git pull -r; cd -; echo; done
}


# #######
# Aliases
# #######

# alias ll="ls -la --color"
# alias llc="ls -l --color"
# alias lsc="ls -a --color"
alias ll="eza -la"
alias ll1="eza -a --long --tree --level=1"
alias ll2="eza -a --long --tree --level=2"
alias llc="eza -l"

alias lsc="eza -a"
alias cls="clear"
alias emd="cd ~/.emacs.d"
alias dmd="cd ~/.doom.d"

# Taken from SO:
# http://stackoverflow.com/questions/13322485/how-to-i-get-the-primary-ip-address-of-the-local-machine-on-linux-and-os-x
alias ipaddress="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"

# what service are running on which ports
alias services_on_ports="lsof -Pnl +M -i4"


alias gdif="git diff --name-only"
alias gdiff="git diff"
alias gtree='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s"'

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'


# #######
# Language
# #######

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi


# #######
# Less
# #######

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi


# #######
# Fzf
# #######

# Set up fzf key bindings and fuzzy completion

# fzf --height 40% --layout reverse
# find bin -type f | fzf -m \
#     --style minimal \
#     --preview 'fzf-preview.sh {}'
# export FZF_DEFAULT_OPTS= '--height 40% --layout reverse -m --style minimal --preview "fzf-preview.sh {}"'

eval "$(fzf --zsh)"
export FZF_DEFAULT_OPTS="
-m
--height 40%
--layout reverse"

alias fz="fzf --style minimal \
    --preview 'fzf-preview.sh {}' \
    --walker-skip .git,node_modules,target"


# #######
# Bat
# #######

# export BAT_THEME="Monokai Extended Light"
export BAT_THEME="Nord"

# git diff
function batdiff  {
    git diff --name-only --relative --diff-filter=d | xargs bat --diff
}

# Man
export MANPAGER="sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"

# Help
alias bathelp='bat --plain --language=help'
function help {
    "$@" --help 2>&1 | bathelp
}


# #######
# Zoxide
# #######
export _ZO_RESOLVE_SYMLINKS=1
eval "$(zoxide init --cmd cd zsh)"


# #######
# Oh My Posh (Prompt)
# #######

# Set the directory we want to store zinit and plugins
OMP="${XDG_DATA_HOME:-${HOME}/.local/share}/oh-my-posh"

# Download Oh My Posh, if it's not there yet
if [ ! -f "$OMP" ]; then
    curl -s https://ohmyposh.dev/install.sh | bash -s -- -d ${HOME}/.local/share
fi

if [ ! -d "$HOME/.config/ohmyposh" ]; then

    mkdir -p "$HOME/.config/ohmyposh"

    # curl -o $HOME/.config/ohmyposh/zen.toml \
    #     "https://raw.githubusercontent.com/dreamsofautonomy/zen-omp/refs/heads/main/zen.toml"
    ln -s ~/Projects/dotfiles/ohmyposh.toml $HOME/.config/ohmyposh/zen.toml
fi

if [ "$TERM_PROGRAM" != "Apple_Terminal" ]; then

    # eval "$(oh-my-posh init zsh)"
    eval "$(oh-my-posh init zsh --config $HOME/.config/ohmyposh/zen.toml)"
fi

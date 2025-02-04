#!/usr/local/bin/bash
# #!/bin/bash
# set -u      # Error on undefined variables
# set -x      # Print commands as they execute
# set -e      # Exit on error
# set -o pipefail # Exit on pipe failures

# Print line number on error
# trap 'echo "Error on line $LINENO"' ERR



# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Disabled
# # Source global definitions
# if [ -f /etc/bashrc ]; then
#   . /etc/bashrc
# fi

# # If not running interactively, don't do anything
# [ -z "$PS1" ] && return
#
# # don't put duplicate lines in the history. See bash(1) for more options
# # ... or force ignoredups and ignorespace
# HISTCONTROL=ignoredups:ignorespace
#
# append to the history file, don't overwrite it
shopt -s histappend


# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# # set variable identifying the chroot you work in (used in the prompt below)
# if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
#     debian_chroot=$(cat /etc/debian_chroot)
# fi
#
# # set a fancy prompt (non-color, unless we know we "want" color)
# case "$TERM" in
#     xterm-color) color_prompt=yes;;
# esac
#
# # uncomment for a colored prompt, if the terminal has the capability; turned
# # off by default to not distract the user: the focus in a terminal window
# # should be on the output of commands, not on the prompt
# #force_color_prompt=yes
#
# if [ -n "$force_color_prompt" ]; then
#     if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
# 	# We have color support; assume it's compliant with Ecma-48
# 	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
# 	# a case would tend to support setf rather than setaf.)
# 	color_prompt=yes
#     else
# 	color_prompt=
#     fi
# fi
#
# if [ "$color_prompt" = yes ]; then
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
# else
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
# fi
# unset color_prompt force_color_prompt
#
# # If this is an xterm set the title to user@host:dir
# case "$TERM" in
# xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#     ;;
# *)
#     ;;
# esac
#
# # enable color support of ls and also add handy aliases
# if [ -x /usr/bin/dircolors ]; then
#     test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
#     alias ls='ls --color=auto'
#     #alias dir='dir --color=auto'
#     #alias vdir='vdir --color=auto'
#
#     alias grep='grep --color=auto'
#     alias fgrep='fgrep --color=auto'
#     alias egrep='egrep --color=auto'
# fi
#


# #######
# Bash Completions
# #######

if [ -f ~/Projects/bash-completion/bash_completion ]; then
    . ~/Projects/bash-completion/bash_completion
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

eval "$(fzf --bash)"
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
eval "$(zoxide init --cmd cd bash)"


# #######
# Prompt, Oh My Posh
# #######
eval "$(oh-my-posh init bash)"

#!/usr/local/bin/bash
# #!/bin/bash
# set -u      # Error on undefined variables
# set -x      # Print commands as they execute
# set -e      # Exit on error
# set -o pipefail # Exit on pipe failures

# Print line number on error
# trap 'echo "Error on line $LINENO"' ERR


# ####
# PATH
# ####

if [ -d "$HOME/.local/bin" ]; then
  PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.bin" ]; then
  PATH="$HOME/.bin:$PATH"
fi

if [ -d "$HOME/bin" ]; then
  PATH="$HOME/bin:$PATH"
fi


# ####
# Navigation
# ####
set -o emacs


# ####
# Turn on extended glob option
# ####
shopt -s extglob


# ####
# Functions
# ####

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


# ####
# Aliases
# ####

alias ll="ls -la --color"
alias llc="ls -l --color"
alias lsc="ls -a --color"
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


# export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \$ '
# export HISTSIZE=
# export HISTFILESIZE=
# export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home"
# export SCALA_HOME="~/Tools/scala-2.11.6"
# export GOPATH=/Users/tim.washington
# export PROJECTS_HOME="~/Projects"
# export M2_HOME="/usr/local/apache-maven-3.3.9"
# export M2_REPO="~/.m2/repository"

# export PATH="\
# ~/bin:\
# ~/.bin:\
# ~/.cask/bin:\
# ~/.cabal/bin:\
# /usr/local/Cellar/emacs/26.3/bin:\
# /sbin:\
# /usr/local/heroku/bin:\
# $JAVA_HOME/bin:\
# $M2_HOME/bin:\
# $GOPATH/bin:\
# $PATH"


# # emacs --daemon > /dev/null 2>&1 &
#
# #gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"
# #if [ -f "${HOME}/.gpg-agent-info" ]; then
# #  . "${HOME}/.gpg-agent-info"
# #  export GPG_AGENT_INFO
# #  export SSH_AUTH_SOCK
# #fi
# #
# #GPG_TTY=$(tty)
# #export GPG_TTY
#
#
# # Invoke GnuPG-Agent the first time we login.
# # Does `~/.gpg-agent-info' exist and points to gpg-agent process accepting signals?
# #if test -f $HOME/.gpg-agent-info && \
# #    kill -0 `cut -d: -f 2 $HOME/.gpg-agent-info` 2>/dev/null; then
# #    GPG_AGENT_INFO=`cat $HOME/.gpg-agent-info | cut -c 16-`
# #else
# #    # No, gpg-agent not available; start gpg-agent
# #    eval `gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info`
# #fi
# #export GPG_TTY=`tty`
# #export GPG_AGENT_INFO
#
#
# # Invoke GnuPG-Agent the first time we login.
# # Does `.gpg-agent-info' exist and points to a gpg-agent process accepting signals?
# # if [ -f $HOME/.gpg-agent-info ] && \
# #        kill -0 $(cut -d: -f 2 $HOME/.gpg-agent-info) 2>/dev/null
# # then
# #     # Yes, `.gpg-agent.info' points to valid gpg-agent process;
# #     # Indicate gpg-agent process
# #     GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info | cut -c 16-)
# # else
# #     # No, no valid gpg-agent process available;
# #     # Start gpg-agent
# #     eval $(gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info)
# # fi
# # export GPG_TTY=$(tty)
# # export GPG_AGENT_INFO
#
# ssh_key_test () {
#   ssh-add -l | grep --silent $1
# }
#
# ensure_ssh_key () {
#   ssh_key_test $1 || ssh-add $1
# }
#
# # ensure_ssh_key ~/.ssh/id_rsa
# # ensure_ssh_key ~/.ssh/uk-team.pem
# # ensure_ssh_key ~/.ssh/nor-cal.pem
# # ensure_ssh_key ~/.ssh/prod-06-10-2016
#
#
# # taken from: http://superuser.com/questions/285381/how-does-the-tmux-color-palette-work
# function print_bash_colors () {
#     for i in {0..255}; do
#   printf "\x1b[38;5;${i}mcolour${i}\x1b[0m\n"
#     done
# }
#

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

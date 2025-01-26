if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if [ -f ~/.local/bin/bashmarks.sh ]; then
    source ~/.local/bin/bashmarks.sh
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

function ubuntu-version {
  lsb_release -a
}
function ubuntu-packages {
  dpkg --get-selections | grep -v deinstall
}
function git-log-tree {
  git log --all --graph --decorate --oneline --simplify-by-decoration
}
function find-in-jar {
  find $1 -name "*.jar" -exec sh -c "echo; echo Inspecting: {}; jar tvf {} | grep $2" \;
}
function switch-emacs {
  rm ~/.emacs.d
  ln -s ~/.emacs.$1 ~/.emacs.d
}

set -o emacs


# turn on extended glob option
shopt -s extglob

export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] \$ '
export HISTSIZE=
export HISTFILESIZE=
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home"
export SCALA_HOME="~/Tools/scala-2.11.6"
export GOPATH=/Users/tim.washington
export PROJECTS_HOME="~/Projects"
export M2_HOME="/usr/local/apache-maven-3.3.9"
export M2_REPO="~/.m2/repository"

export PATH="\
~/bin:\
~/.bin:\
~/local/bin:\
~/.cask/bin:\
~/.cabal/bin:\
~/local/bin:\
/usr/local/Cellar/emacs/26.3/bin:\
/sbin:\
/usr/local/heroku/bin:\
$JAVA_HOME/bin:\
$M2_HOME/bin:\
$GOPATH/bin:\
$PATH"

export BAT_THEME="Monokai Extended Light"

source /Users/timothyw/.nix-profile/etc/profile.d/nix.sh


# Taken from SO:
# http://stackoverflow.com/questions/13322485/how-to-i-get-the-primary-ip-address-of-the-local-machine-on-linux-and-os-x
alias ipaddress="ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p'"


alias em="emacs"
alias ec="emacsclient"
alias emacsc="emacsclient"
alias emd="cd ~/.emacs.d"


# take from here: https://twitter.com/puffnfresh/status/568439637846261760
alias ll="ls -la --color"
alias llc="ls -l --color"
alias lsc="ls -aG --color"
alias cls="clear"
alias gk="g bk"
alias gkp="g bkp"
alias gnn="g nn"
alias gt="g tls"
alias gb="g bin"
alias gp="g prj"
alias gd="g dwn"
alias gi="g in"
alias gip="g ip"

alias ebashp="vim ~/.bash_profile"
alias evimrc="vim ~/.vimrc"
alias eprofiles="vim ~/.lein/profiles.clj"
alias lrepl="lein repl"
alias rf="rm -rf"
alias gdif="git diff --name-only"
alias gdiff="git diff"
alias gtree='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s"'
alias pinentry="pinentry-curses"
alias grepr="grep -r"
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

function echon {
  echo $1 | tr $2 '\n'
}

# what service are running on which ports
alias services_on_ports="lsof -Pnl +M -i4"


#function sea { apt-cache search $1 | grep -C 500 $1; }
#alias sea2="sea --names-only"
#function ins { sudo apt-get install -y $* && aptn "Installed $@" }
#function rem { sudo apt-get remove -y $* && aptn "Removed $@" }
#alias upd="sudo apt-get update"



# EXAMPLES
#
# removing sub-directories in git
# git update-index --force-remove <your-dir>

# return uncommitted working tree to the last committed state with
# git reset --hard HEAD
# git reset HEAD somefile.txt

function _update_ps1() {
    PS1="$(~/powerline-shell.py $? 2> /dev/null)"
}

if [ "$TERM" != "linux" ]; then
    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
fi


POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1

LANG="en_US.UTF-8"
LC_COLLATE="en_US.UTF-8"
LC_CTYPE="en_US.UTF-8"
LC_MESSAGES="en_US.UTF-8"
LC_MONETARY="en_US.UTF-8"
LC_NUMERIC="en_US.UTF-8"
LC_TIME="en_US.UTF-8"
LC_ALL="en_US.UTF-8"

export POWERLINE_CONFIG_COMMAND="powerline-config"
export POWERLINE_HOME="~/Library/Python/2.7/lib/python/site-packages/powerline"
if [ -f $POWERLINE_HOME/bindings/bash/powerline.sh ]; then
    source $POWERLINE_HOME/bindings/bash/powerline.sh
fi


# emacs --daemon > /dev/null 2>&1 &

#gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"
#if [ -f "${HOME}/.gpg-agent-info" ]; then
#  . "${HOME}/.gpg-agent-info"
#  export GPG_AGENT_INFO
#  export SSH_AUTH_SOCK
#fi
#
#GPG_TTY=$(tty)
#export GPG_TTY


# Invoke GnuPG-Agent the first time we login.
# Does `~/.gpg-agent-info' exist and points to gpg-agent process accepting signals?
#if test -f $HOME/.gpg-agent-info && \
#    kill -0 `cut -d: -f 2 $HOME/.gpg-agent-info` 2>/dev/null; then
#    GPG_AGENT_INFO=`cat $HOME/.gpg-agent-info | cut -c 16-`
#else
#    # No, gpg-agent not available; start gpg-agent
#    eval `gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info`
#fi
#export GPG_TTY=`tty`
#export GPG_AGENT_INFO


# Invoke GnuPG-Agent the first time we login.
# Does `.gpg-agent-info' exist and points to a gpg-agent process accepting signals?
# if [ -f $HOME/.gpg-agent-info ] && \
#        kill -0 $(cut -d: -f 2 $HOME/.gpg-agent-info) 2>/dev/null
# then
#     # Yes, `.gpg-agent.info' points to valid gpg-agent process;
#     # Indicate gpg-agent process
#     GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info | cut -c 16-)
# else
#     # No, no valid gpg-agent process available;
#     # Start gpg-agent
#     eval $(gpg-agent --daemon --no-grab --write-env-file $HOME/.gpg-agent-info)
# fi
# export GPG_TTY=$(tty)
# export GPG_AGENT_INFO

ssh_key_test () {
  ssh-add -l | grep --silent $1
}

ensure_ssh_key () {
  ssh_key_test $1 || ssh-add $1
}

# ensure_ssh_key ~/.ssh/id_rsa
# ensure_ssh_key ~/.ssh/uk-team.pem
# ensure_ssh_key ~/.ssh/nor-cal.pem
# ensure_ssh_key ~/.ssh/prod-06-10-2016


# taken from: http://superuser.com/questions/285381/how-does-the-tmux-color-palette-work
function print_bash_colors () {
    for i in {0..255}; do
  printf "\x1b[38;5;${i}mcolour${i}\x1b[0m\n"
    done
}

# taken from: https://twitter.com/_komaz/status/514628905576460288
function mkcd () {
    mkdir -p "$1" && cd "$1"
}

# taken from: http://cfenollosa.com/misc/tricks.txt
function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }

source ~/.local/bin/bashmarks.sh

# USAGE:
# ------
# s <bookmark_name> - Saves the current directory as "bookmark_name"
# g <bookmark_name> - Goes (cd) to the directory associated with "bookmark_name"
# p <bookmark_name> - Prints the directory associated with "bookmark_name"
# d <bookmark_name> - Deletes the bookmark
# l                 - Lists all available bookmarks

# Docker configuration
# eval $(docker-machine env default)
function docker-remove-untagged-images() {
  docker rmi -f $(docker images | grep "^<none>" | awk '{print $3}')
}

function docker-clear-containers() {
  docker rm -f $(docker ps -a -q)
}

function docker-clear-images() {
  docker rmi -f $(docker images -a -q)
}

function docker-clear-volumes() {
  docker volume rm $(docker volume ls -q)
}

function docker-clear-networks() {
  docker network rm $(docker network ls | tail -n+2 | awk '{if($2 !~ /bridge|none|host/){ print $1 }}')
}

function docker-attach-to-container() {
  docker exec -i -t $1 /bin/bash
}

function docker-container-ip-address() {
  docker inspect $1
}

function git-pull-all() {
  for i in `ls`; do echo "Pulling: $i"; cd $i; git pull -r; cd -; echo; done
}

complete -C aws_completer aws
if [ -e /Users/timothyw/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/timothyw/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
PATH=$PATH:/opt/metasploit-framework/bin
export PATH=$PATH:/opt/metasploit-framework/bin

GUIX_PROFILE="$HOME/.guix-profile" ; \
source "$GUIX_PROFILE/etc/profile"

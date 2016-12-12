#!/bin/zsh

# Apparently this enables autocompletion
autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Case insensitive auto completion


# emacs mode
bindkey -e

#export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='emacsclient'
fi

# Delete a word until a /
backward-kill-dir () {
    local WORDCHARS=${WORDCHARS/\/}
    zle backward-kill-word
}
zle -N backward-kill-dir
bindkey '' backward-kill-dir

# start keychain
eval $(keychain --eval --quiet id_rsa)

# Prompt
PROMPT=$'%K{cyan} %F{black}%n@%m%f %k%B%K{blue} %~ %k%b '

#
# Aliases 
#

# ls with colors
alias ls='ls --color=auto'
# Emacs
alias ec="emacsclient -n"
#matlab
alias matlabc='matlab -nodesktop -nosplash'
# Taskwarrior aliases
alias japanese='task add +japanese'
alias systems='task add +systems'
alias linear='task add +linear'
alias discrete='task add +discrete'
alias todo='task +D'

# Git add, commit, and push in one command
lazygit() {
    git add .
    git commit -a -m "$1"
    git push
}

# Syncing taskwarrior
lazytask() {
    old_dir=$(pwd)
    if [[ "$old_dir" != "$HOME/gitrepos/notProgramming" ]]; then
        cd ~/gitrepos/notProgramming
    fi
    git add .
    git commit -a -m "tasks"
    git push
    if [[ "$old_dir" != "$HOME/gitrepos/notProgramming" ]]; then
        cd -
    fi
}

# Check all the git repos
cherry() {
    no_updates='nothing to commit, working tree clean'
    old_dir=$(pwd)
    for repo in ~/gitrepos/*; do
        cd $repo
        if [[ ! $(git status | grep $no_updates) ]]; then
            echo $repo
        fi
    done
    cd ~/dotfiles
    if [[ ! $(git status | grep $no_updates) ]]; then
	pwd
    fi
    cd $old_dir
}

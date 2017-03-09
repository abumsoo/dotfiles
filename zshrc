#!/bin/zsh

# Apparently this enables autocompletion
autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Case insensitive auto completion

# Zsh git prompt
source $HOME/dotfiles/zsh-git-prompt/zshrc.sh

# emacs mode
bindkey -e

export PATH=$PATH:$HOME/.gem/ruby/2.4.0/bin

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
PROMPT=$'%m:%B%F{blue}%~%f%b$(git_super_status)$ '

# ALIASES
# ls with colors
alias ls='ls --color=auto'
# Emacs
alias ec="emacsclient -n"
#matlab
alias matlabc='matlab -nodesktop -nosplash'
# Taskwarrior aliases
alias japanese='task add +japanese'
alias software='task add +software'
alias chem='task add +chem'
alias chemlab='task add +chemlab'
alias discrete='task add +discrete'
alias td='task +discrete or +chem or +chemlab or +japanese or +software'
alias aoeu='more ~/Dropbox/actions'

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

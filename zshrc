
  export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='emacsclient'
fi

# start keychain
eval $(keychain --eval --quiet id_rsa)

# Aliases
alias ec="emacsclient -n"
alias matlabc='matlab -nodesktop -nosplash'
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

cherry() {
    no_updates='nothing to commit, working tree clean'
    old_dir=$(pwd)
    for repo in ~/gitrepos/*; do
        cd $repo
        if [[ ! $(git status | grep $no_updates) ]]; then
            echo $repo
        fi
    done
    cd $old_dir
}

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Emacs mode
bindkey -e

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/bumshakabum/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -Uz promptinit
promptinit

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#PROMPT=$'%F{blue}%n%f@%F{blue}%m%f:[%F{green}%~%f]\n> '
PROMPT=$'%B%K{blue}%F{white} %~%f %k%b%F{blue}\ue0b0%f '

export PATH=/home/MATLAB/R2016a/bin:$PATH

eval `keychain --eval id_rsa`

alias ec='emacsclient -n'
alias octave='octave --no-gui'
alias ls='ls --color=auto'
alias ll='ls -la'

lazygit(){
    git add .
    git commit -a -m $1
    git push
}

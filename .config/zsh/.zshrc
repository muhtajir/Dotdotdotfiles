setopt appendhistory autocd extendedglob correct share_history prompt_subst\
    glob_complete
unsetopt beep
zmodload zsh/complist
zstyle :compinstall filename '$ZDOTDIR/.zshrc'
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
zstyle ':completion:*' rehash true

WORDCHARS=''

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz colors
colors

autoload -Uz copy-earlier-word
zle -N copy-earlier-word

# source additional stuff
source $ZDOTDIR/.zfunc
source $ZDOTDIR/.zprompt

# ring terminal bell on longer tasks
# Written by Jean-Philippe Ouellet <jpo@vt.edu>
# Made available under the ISC license.
# split into different files and simplified
zmodload zsh/datetime
zbell_timestamp=$EPOCHSECONDS

# create a navigation history with pushd
setopt autopushd pushdminus pushdsilent
alias dc='dirs -v'

# widgets for history searching with arrow keys
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Aliases and export options
alias sudo='sudo ' # working aliases following sudo
alias ls='ls --color=auto'
alias pacupdate='python2 ~/Etc./Scripts/pacupdate.py'
alias locate='find / -not \( -path /dev -prune \) -not \( -path /proc -prune \) -not \( -path /sys -prune \) -not \( -path /run -prune \) -not \( -path /mnt -prune \) -not \( -path /media -prune \) -not \( -path /lost+found -prune \) -iname $* 2>/dev/null'
alias cls='echo -ne "\033c"'
alias bootwin='sudo efibootmgr -n 0000 && systemctl reboot'
alias v='nvim'
alias vim='nvim'
alias sctl='systemctl'
alias sctlu='systemctl --user'
alias mksri='makepkg -sri'
alias q='exit'

export HISTFILE="$ZDOTDIR/.zsh_history"
export KEYTIMEOUT=1
export HISTSIZE=3500
export SAVEHIST=7000
export HISTCONTROL=ignoredups
export EDITOR='nvim'
export media="/run/media/${USER}"
export aur="${HOME}/Downloads/AUR"
export scripts="${HOME}/Etc./Scripts"

# KEYBINDINGS
bindkey -e

# special key aliases
typeset -A key
key[Home]="^[[H"
key[End]="^[[F"
key[Insert]="^[[2~"
key[Delete]="^[[3~"
key[PageUp]="^[[5~"
key[PageDown]="^[[6~"
key[Up]="^[[A"
key[Down]="^[[B"
key[Left]="^[[D"
key[Right]="^[[C"

# modifier aliases… i can never remember these
typeset -A mod
mod[Alt]="^["
mod[Ctrl]="^"

bindkey "${key[Home]}" beginning-of-line
bindkey "${key[End]}" end-of-line
bindkey "${key[Insert]}" overwrite-mode
bindkey "${key[Delete]}" delete-char
bindkey "${key[Up]}" up-line-or-history
bindkey "${key[Down]}" down-line-or-history
bindkey "${key[Left]}" backward-char
bindkey "${key[Right]}" forward-char
bindkey "${mod[Ctrl]}H" vi-backward-kill-word
bindkey "${key[PageUp]}" up-line-or-search
bindkey "${key[PageDown]}" down-line-or-search
bindkey "^[[1;5D" vi-backward-word
bindkey "^[[1;5C" vi-forward-word
bindkey "^[[1;3D" vi-backward-word
bindkey "^[[1;3C" vi-forward-word
bindkey "${key[Up]}"   up-line-or-beginning-search
bindkey "${key[Down]}" down-line-or-beginning-search
bindkey "${mod[Alt]}k"   up-line-or-beginning-search
bindkey "${mod[Alt]}j" down-line-or-beginning-search
bindkey "${mod[Ctrl]}p" history-incremental-search-backward
bindkey "${mod[Ctrl]}n" history-incremental-search-forward

# add vi mode to emacs bindings
bindkey "^[" vi-cmd-mode

# vi keys in menu select
bindkey -M menuselect j down-line-or-history
bindkey -M menuselect k up-line-or-history
bindkey -M menuselect l forward-char
bindkey -M menuselect h backward-char

bindkey "${mod[Alt]}m" copy-earlier-word

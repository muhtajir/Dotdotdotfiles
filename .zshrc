setopt appendhistory autocd extendedglob complete_aliases
unsetopt beep
zstyle :compinstall filename '/home/nicolai/.zshrc'
zstyle ':completion:*' menu select

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz colors
colors

PS1="%F{1}░▒▓█%K{1}%F{0}%# %F{1}%K{2}%F{0} %1~ %k%F{2} ∙%f "

# Aliases and export options
alias ls='ls --color=auto'
alias pacupdate='python2 ~/Etc./Scripts/pacupdate.py'
alias locate='find / -not \( -path /dev -prune \) -not \( -path /proc -prune \) -not \( -path /sys -prune \) -not \( -path /run -prune \) -not \( -path /mnt -prune \) -not \( -path /media -prune \) -not \( -path /lost+found -prune \) -iname $* 2>/dev/null'
alias cls='echo -ne "\033c"'
alias bootwin='sudo efibootmgr -n 0000 && systemctl reboot'

HISTFILE=~/.histfile
HISTSIZE=3500
SAVEHIST=7000
HISTCONTROL=ignoredups
aur="${HOME}/Downloads/AUR"
scripts="${HOME}/Etc./Scripts"

# make special keys work
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}

bindkey "${key[Home]}" beginning-of-line
bindkey "${key[End]}" end-of-line
bindkey "${key[Insert]}" overwrite-mode
bindkey "${key[Delete]}" delete-char
bindkey  "${key[Up]}"      up-line-or-history
bindkey  "${key[Down]}"    down-line-or-history
bindkey  "${key[Left]}"    backward-char
bindkey  "${key[Right]}"   forward-char
bindkey "^\b" backward-kill-word
bindkey "${key[PageUp]}" up-line-or-search
bindkey "${key[PageDown]}" down-line-or-search
bindkey "^[[1;5D" emacs-backward-word
bindkey "^[[1;5C" emacs-forward-word

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        echoti smkx
    }
    function zle-line-finish () {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

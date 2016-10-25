#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

eval $(dircolors -p | sed -r 's/;42//g' | dircolors -)

#PS1='\[\e[1;92m\]\u\[\e[0m\]@\[\e[1;37m\]\h\[\e[0m\] \W \$ '
#PS1='\[\e[1;32m\]\u\[\e[0m\]@\[\e[1m\]\h\[\e[0m\] \[\e[36m\]\W \[\e(b\e[m\]\$ '

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

alias ls='ls --color=auto'
alias pacupdate='python2 ~/Etc./Scripts/pacupdate.py'
alias locate='find / -not \( -path /dev -prune \) -not \( -path /proc -prune \) -not \( -path /sys -prune \) -not \( -path /run -prune \) -not \( -path /mnt -prune \) -not \( -path /media -prune \) -not \( -path /lost+found -prune \) -iname $* 2>/dev/null'
alias cls='echo -ne "\033c"'
alias bootwin='sudo efibootmgr -n 0000 && systemctl reboot'

export PS1="\[$(tput bold)\]\[\033[38;5;2m\]\u\[$(tput sgr0)\]\[$(tput sgr0)\]\[\033[38;5;15m\]@\[$(tput bold)\]\h\[$(tput sgr0)\] \[$(tput sgr0)\]\[\033[38;5;6m\]\W\[$(tput sgr0)\]\[\033[38;5;15m\] \\$ \[$(tput sgr0)\]"
export HISTCONTROL=ignoredups
export HISTSIZE=5000
export aur='/home/nicolai/Downloads/AUR'
export scripts='/home/nicolai/Etc./Scripts'

setopt appendhistory autocd extendedglob
unsetopt beep
zstyle :compinstall filename '/home/nicolai/.zshrc'

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz colors
colors

PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%1~ %{$reset_color%}%# "

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
aur='/home/nicolai/Downloads/AUR'
scripts='/home/nicolai/Etc./Scripts'

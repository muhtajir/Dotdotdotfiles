setopt appendhistory autocd extendedglob complete_aliases correct prompt_subst
unsetopt beep
zstyle :compinstall filename '$ZDOTDIR/.zshrc'
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}' '+m:{A-Z}={a-z}'

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*+set-message:*' hooks blank_master
zstyle ':vcs_info:*+no-vcs:*' hooks no_vcs_prompt
function +vi-no_vcs_prompt() {
    vcs_info_msg_0_="%F{4}░▒▓%K{4}%F{0} %(0?,%# %F{4}%K{2},%? %F{2}%K{4}%K{2})%F{0} %1~ %k%F{2} "
}
function +vi-blank_master() {
    if [[ ${hook_com[branch_orig]} == 'master' ]]; then
        hook_com[branch]=''
    fi
}
zstyle ':vcs_info:git:*' formats "%F{5}░▒▓%K{5}%F{0} %(0?,%b %F{5}%K{2},%? %F{2}%K{5}%K{2})%F{0} %1~ %k%F{2} "

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz colors
colors

# create a navigation history with pushd
setopt autopushd pushdminus pushdsilent
alias dc='dirs -v'

# widgets for history searching with arrow keys
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# over-the-top prompt theme
[[ $TERM == "xterm-termite" ]] && PS1='${vcs_info_msg_0_}%(0?,∙,%F{1}∙)%f '
[[ $TERM == "xterm-termite" ]] && PS2="%F{3}░▒▓%K{3}%F{0} %_ %k%F{3}%f "

# Aliases and export options
alias ls='ls --color=auto'
alias pacupdate='python2 ~/Etc./Scripts/pacupdate.py'
alias locate='find / -not \( -path /dev -prune \) -not \( -path /proc -prune \) -not \( -path /sys -prune \) -not \( -path /run -prune \) -not \( -path /mnt -prune \) -not \( -path /media -prune \) -not \( -path /lost+found -prune \) -iname $* 2>/dev/null'
alias cls='echo -ne "\033c"'
alias bootwin='sudo efibootmgr -n 0000 && systemctl reboot'

export HISTFILE=~/.histfile
export HISTSIZE=3500
export SAVEHIST=7000
export HISTCONTROL=ignoredups
export aur="${HOME}/Downloads/AUR"
export scripts="${HOME}/Etc./Scripts"
export EDITOR='vim'

function precmd {
    vcs_info
    print -Pn "\e]0;Ter--[ %c ]--mite\a"
}

function preexec {
    printf "\033]0;%s\a" "Ter--{ $1 }--mite"
}

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

[[ -n "${key[Home]}" ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n "${key[End]}" ]] && bindkey "${key[End]}" end-of-line
[[ -n "${key[Insert]}" ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n "${key[Delete]}" ]] && bindkey "${key[Delete]}" delete-char
[[ -n "${key[Up]}" ]] && bindkey  "${key[Up]}" up-line-or-history
[[ -n "${key[Down]}" ]] && bindkey  "${key[Down]}" down-line-or-history
[[ -n "${key[Left]}" ]] && bindkey  "${key[Left]}" backward-char
[[ -n "${key[Right]}" ]] && bindkey  "${key[Right]}" forward-char
[[ -n "^\b" ]] && bindkey "^\b" vi-backward-kill-word
[[ -n "${key[PageUp]}" ]] && bindkey "${key[PageUp]}" up-line-or-search
[[ -n "${key[PageDown]}" ]] && bindkey "${key[PageDown]}" down-line-or-search
[[ -n "^[[1;5D" ]] && bindkey "^[[1;5D" vi-backward-word
[[ -n "^[[1;5C" ]] && bindkey "^[[1;5C" vi-forward-word
[[ -n "^[[1;3D" ]] && bindkey "^[[1;3D" vi-backward-word
[[ -n "^[[1;3C" ]] && bindkey "^[[1;3C" vi-forward-word
[[ -n "${key[Up]}"   ]] && bindkey "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

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

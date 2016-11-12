setopt appendhistory autocd extendedglob complete_aliases correct share_history\
    prompt_subst glob_complete
unsetopt beep
zmodload zsh/complist
zstyle :compinstall filename '$ZDOTDIR/.zshrc'
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}' '+m:{A-Z}={a-z}'

# function to check in what kind of terminal we are
function is_pts() {
    tstyle=$(tty)
    if [[ ${tstyle:5:3} == pts ]];then
        return 0
    else
        return 1
    fi
}

# OVERLY ELABORATE PROMPT SETUP STARTS HERE
# first, a fall back prompt if is_pts returns false
PS1="%B%F{2}%n%f@%M%b %1~ %#"
# functions that define parts of the prompt, color is first argument,
# second argument is content
function p1_prompt() {
    echo "%F{${1}}░▒▓%K{${1}}%F{0} %(0?,${2},%?) %F{${1}}"
}
function p2_prompt() {
echo "%K{${1}}%F{0} ${2} %k%F{${1}} "
}
function p3_prompt() {
    echo "%F{${1}}∙%f "
}
# for the time being, only a semi-fancy PS2-prompt
is_pts && PS2='$(p1_prompt 3 %_)%k%F{3}%f '
# enable vcs_info for git only and never print master branch
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*+set-message:*' hooks blank_master
# workaround for broken nvcsformats
# set prompt at vcs_info start-up, overwrite if there's no vcs
zstyle ':vcs_info:*+no-vcs:*' hooks no_vcs_prompt
zstyle ':vcs_info:*+start-up:*' hooks vcs_prompt
function +vi-vcs_prompt() {
    is_pts && PS1='$(p1_prompt 5 ${vcs_info_msg_0_})$(p2_prompt 2 %1~)$(p3_prompt 2)'
}
function +vi-no_vcs_prompt() {
is_pts && PS1='$(p1_prompt 4 "%#")$(p2_prompt 2 %1~)$(p3_prompt 2)'
}
function +vi-blank_master() {
    if [[ ${hook_com[branch_orig]} == 'master' ]]; then
        hook_com[branch]=''
    fi
}
zstyle ':vcs_info:git:*' formats "%b"
# OVERLY ELABORATE PROMPT SETUP ENDS HERE

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz colors
colors

autoload -Uz copy-earlier-word
zle -N copy-earlier-word

# create a navigation history with pushd
setopt autopushd pushdminus pushdsilent
alias dc='dirs -v'

# widgets for history searching with arrow keys
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Aliases and export options
alias ls='ls --color=auto'
alias pacupdate='python2 ~/Etc./Scripts/pacupdate.py'
alias locate='find / -not \( -path /dev -prune \) -not \( -path /proc -prune \) -not \( -path /sys -prune \) -not \( -path /run -prune \) -not \( -path /mnt -prune \) -not \( -path /media -prune \) -not \( -path /lost+found -prune \) -iname $* 2>/dev/null'
alias cls='echo -ne "\033c"'
alias bootwin='sudo efibootmgr -n 0000 && systemctl reboot'

export HISTFILE=~/.histfile
export KEYTIMEOUT=1
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

bindkey "${key[Home]}" beginning-of-line
bindkey "${key[End]}" end-of-line
bindkey "${key[Insert]}" overwrite-mode
bindkey "${key[Delete]}" delete-char
bindkey  "${key[Up]}" up-line-or-history
bindkey  "${key[Down]}" down-line-or-history
bindkey  "${key[Left]}" backward-char
bindkey  "${key[Right]}" forward-char
bindkey "^\b" vi-backward-kill-word
bindkey "${key[PageUp]}" up-line-or-search
bindkey "${key[PageDown]}" down-line-or-search
bindkey "^[[1;5D" vi-backward-word
bindkey "^[[1;5C" vi-forward-word
bindkey "^[[1;3D" vi-backward-word
bindkey "^[[1;3C" vi-forward-word
bindkey "${key[Up]}"   up-line-or-beginning-search
bindkey "${key[Down]}" down-line-or-beginning-search
bindkey "^[k"   up-line-or-beginning-search
bindkey "^[j" down-line-or-beginning-search
# nonsensically add vi mode to emacs bindings
bindkey "^[" vi-cmd-mode
# vi keys in menu select
bindkey -M menuselect j down-line-or-history
bindkey -M menuselect k up-line-or-history
bindkey -M menuselect l forward-char
bindkey -M menuselect h backward-char
bindkey "^[m" copy-earlier-word

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

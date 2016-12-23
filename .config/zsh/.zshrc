setopt appendhistory autocd extendedglob correct share_history prompt_subst\
    glob_complete
unsetopt beep
zmodload zsh/complist
zstyle :compinstall filename '$ZDOTDIR/.zshrc'
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
zstyle ':completion:*' rehash true

WORDCHARS=''

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
# set the standard prompts for is_pts=true and is_pts=false. we need the
# fallback prompt for the nvcsformats workaround
ST_PROMPT='$(p1_prompt 4 "%#")$(p2_prompt 2 ${p_location})$(p3_prompt 2)'
FB_PROMPT='%B%F{2}%n%f@%M%b ${p_location} %# '
VCS_PROMPT='$(p1_prompt 5 $(git_change_symbol)${vcs_info_msg_0_})$(p2_prompt 2 ${p_location})$(p3_prompt 2)'
if is_pts; then
    PS1=$ST_PROMPT
else
    PS1=$FB_PROMPT
fi
# functions/variables that define parts of the prompt,the first argument is
# color, the second argument is content
p_location="%15>…>%1~%>>"
function p1_prompt() {
    if [[ -n $M_PROMPT ]]; then
        echo "%F{16}░▒▓%K{16}%F{0} ${M_PROMPT} %F{16}"
    else
        echo "%F{${1}}░▒▓%K{${1}}%F{0} %(0?,${2},%?) %F{${1}}"
    fi
}
function p2_prompt() {
echo "%K{${1}}%F{0} ${2} %k%F{${1}} "
}
function p3_prompt() {
    if [[ -n $M_PROMPT ]]; then
        echo "%F{16}∙%f "
    else
        echo "%(0?,%F{${1}},%F{1})∙%f "
    fi
}
function git_change_symbol() {
    [[ -n $(git status --porcelain 2>/dev/null) ]] && echo '*'
}
is_pts && PS2='$(p1_prompt 4 "%#")$(p2_prompt 3 %_)$(p3_prompt 3)'
# enable vcs_info for git only and never print master branch
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*+set-message:*' hooks blank_master
# workaround for broken nvcsformats
# set prompt at vcs_info start-up, overwrite if there's no vcs
zstyle ':vcs_info:*+no-vcs:*' hooks no_vcs_prompt
zstyle ':vcs_info:*+post-backend:*' hooks vcs_prompt
function +vi-vcs_prompt() {
    if [[ ${hook_com[base]} = '/home/nicolai' ]]; then
        if [[ $(pwd) = '/home/nicolai' ]]; then
            is_pts && PS1=$VCS_PROMPT
        else
            is_pts && PS1=$ST_PROMPT
        fi
    else
        is_pts && PS1=$VCS_PROMPT
    fi
}
function +vi-no_vcs_prompt() {
    is_pts && PS1='$(p1_prompt 4 "%#")$(p2_prompt 2 ${p_location})$(p3_prompt 2)'
}
function +vi-blank_master() {
    if [[ ${hook_com[branch_orig]} == 'master' ]]; then
        hook_com[branch]=''
    fi
}
zstyle ':vcs_info:git:*' formats "%b"
# indicate if vicmd keymap is active
function zle-keymap-select zle-line-init {
M_PROMPT="${${KEYMAP/vicmd/Φ}/(viins|emacs|main)/}"
zle reset-prompt
}
zle -N zle-line-init zle-keymap-select
zle -N zle-keymap-select
zle -N zle-line-init
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
alias sudo='sudo ' # working aliases following sudo
alias ls='ls --color=auto'
alias pacupdate='python2 ~/Etc./Scripts/pacupdate.py'
alias locate='find / -not \( -path /dev -prune \) -not \( -path /proc -prune \) -not \( -path /sys -prune \) -not \( -path /run -prune \) -not \( -path /mnt -prune \) -not \( -path /media -prune \) -not \( -path /lost+found -prune \) -iname $* 2>/dev/null'
alias cls='echo -ne "\033c"'
alias bootwin='sudo efibootmgr -n 0000 && systemctl reboot'
alias v='nvim'
alias vim='nvim'
alias sctl='systemctl'
alias mksri='makepkg -sri'

export HISTFILE=~/.histfile
export KEYTIMEOUT=1
export HISTSIZE=3500
export SAVEHIST=7000
export HISTCONTROL=ignoredups
export aur="${HOME}/Downloads/AUR"
export scripts="${HOME}/Etc./Scripts"
export EDITOR='nvim'

function precmd {
    vcs_info
    print -Pn "\e]0;Ter--[ %c ]--mite\a"
}

function preexec {
    printf "\033]0;%s\a" "Ter--{ $1 }--mite"
}

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

# add vi mode to emacs bindings
bindkey "^[" vi-cmd-mode

# vi keys in menu select
bindkey -M menuselect j down-line-or-history
bindkey -M menuselect k up-line-or-history
bindkey -M menuselect l forward-char
bindkey -M menuselect h backward-char

bindkey "${mod[Alt]}m" copy-earlier-word

function __fish_fzf_bind --description 'Wrapper for fzf as a key bind.'
    set -l fzf_result (fzf --bind alt-j:down,alt-k:up --reverse)
    commandline -i "$fzf_result"
    commandline -f repaint
end


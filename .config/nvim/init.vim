call plug#begin()
    Plug 'dietsche/vim-lastplace'
    Plug 'scrooloose/syntastic'
    Plug 'raimondi/delimitmate'
    Plug 'vis'
    Plug 'vim-airline/vim-airline'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
call plug#end()

"functions
"thanks vimcasts.org
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

if has("gui_running")
    set guifont=mononoki\ 12
    set lines=45 columns=160
endif

let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme base16-materia-glass
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set number
set autoindent
set linespace=3
set showcmd
set mouse=a
set autochdir
set ttimeout
set ttimeoutlen=10
set linebreak
set hidden
set hlsearch
set ignorecase
set smartcase
set relativenumber
"use pipe character as cursor in insert mode
let &t_SI .= "\<Esc>[5 q"
let &t_EI .= "\<Esc>[0 q"
"and in neovim as well
:let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
"source the .vimrc/init.vim automatically after saving
if has("autocmd")
    autocmd bufwritepost .vimrc source $MYVIMRC | AirlineRefresh
endif
if has("autocmd")
    autocmd bufwritepost init.vim source $MYVIMRC | AirlineRefresh
endif

""keybinds
let mapleader=','
nnoremap Y y$
"buffer navigation
nnoremap <silent> <leader><Tab> :bn<CR>
nnoremap <silent> <leader><S-Tab> :bp<CR>
nnoremap <silent> <leader>d :bd<CR>
nnoremap <leader>rc :vsplit $MYVIMRC<CR>
"split commands closer to i3
nnoremap <silent> <leader>hs :vsplit<CR>
nnoremap <silent> <leader>vs :split<CR>
"auto-delete trailing whitespace
nnoremap <silent> <F3> :call <SID>StripTrailingWhitespaces()<CR>
"toggle search highlighting on
nnoremap <silent> <F4> :set hlsearch!<CR>
"pseudo-toggle syntaxcheck
nnoremap <silent> <F5> :SyntasticCheck<CR>
nnoremap <silent> <F6> :SyntasticReset<CR>
" insert blank links without entering insert mode
nnoremap ü o<ESC>
nnoremap Ü O<ESC>
"system clipboard accesible by prepending leader key
nnoremap <leader>y "+y
nnoremap <leader>p "+p
nnoremap <leader>Y "+y$
nnoremap <leader>P "+P
"kill text (don't move to register)
nnoremap <leader>k "_d
nnoremap <leader>kk "_dd
nnoremap <leader>K "_D
nnoremap <leader>kiw "_diw
nnoremap <leader>kaw "_daw
"window navigation with alt key
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l
"also in terminal mode
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
"jump to end of line in insert mode
inoremap <C-A> <Esc>A
"use up-down bindings from my zsh configuration
cnoremap <A-k> <Up>
cnoremap <A-j> <Down>

"syntastic configuration
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_pylint_exec = 'pylint2'
let g:syntastic_auto_jump = 3
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }

"vim-airline configuration
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

syntax on

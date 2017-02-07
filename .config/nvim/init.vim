call plug#begin()
    Plug 'dietsche/vim-lastplace'
    Plug 'neomake/neomake'
    Plug 'raimondi/delimitmate'
    Plug 'vim-airline/vim-airline'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-repeat'
    Plug 'mtth/scratch.vim'
    Plug 'vim-scripts/ReplaceWithRegister'
    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-indent'
call plug#end()

"load included matchit_plugin
runtime macros/matchit.vim

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
"don't delete trailing whitespace here
nnoremap <leader>e :e 
nnoremap <leader>rc :vsplit $MYVIMRC<CR>
"split commands closer to i3
nnoremap <silent> <leader>hs :vsplit<CR>
nnoremap <silent> <leader>vs :split<CR>
"auto-delete trailing whitespace
nnoremap <silent> <F3> /\s\+$<CR>
nnoremap <silent> <leader><F3> :call <SID>StripTrailingWhitespaces()<CR>
"turn off search highlighting until next search
nnoremap <silent> <F4> :nohlsearch<CR>
"run Neomake
nnoremap <silent> <F5> :Neomake<CR>
" insert blank links without entering insert mode
nnoremap ü o<ESC>
nnoremap Ü O<ESC>
"system clipboard accesible by prepending leader key
nnoremap <leader>y "+y
nnoremap <leader>p "+p
nnoremap <leader>Y "+y$
nnoremap <leader>P "+P
nmap <leader>gr "+gr
nmap <leader>gR "+gR
"kill text (don't move to register)
nnoremap <leader>k "_d
nnoremap <leader>kk "_dd
nnoremap <leader>K "_D
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

syntax on

"" plugin configuration
"neomake
let g:neomake_python_enabled_makers = ['flake8']

"vim-airline
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

"scratch
let g:scratch_insert_autohide = 0

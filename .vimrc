call plug#begin()
    Plug 'scrooloose/syntastic'
    Plug 'raimondi/delimitmate'
    Plug 'vis'
    Plug 'vim-airline/vim-airline'
    Plug 'tpope/vim-surround'
call plug#end()

if has("gui_running")
    set guifont=mononoki\ 12
    set lines=45 columns=160
endif

colorscheme base16-seti-ui
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
set clipboard=unnamedplus
set hlsearch
set smartcase
"use pipe character in insert mode
let &t_SI .= "\<Esc>[5 q"
let &t_EI .= "\<Esc>[0 q"
"source the .vimrc automatically after saving
if has("autocmd")
    autocmd bufwritepost .vimrc source $MYVIMRC
endif

"keybinds
let mapleader=','
nmap <silent> <leader><Tab> :bn<CR>
nmap <silent> <leader><S-Tab> :bp<CR>
nmap <silent> <C-F10> :w<CR> :so %<CR>
nmap <silent> <F4> :set hlsearch!<CR>
imap <C-A> <Esc>A
nnoremap <silent> <F5> :SyntasticCheck<CR>
nnoremap <silent> <F6> :SyntasticReset<CR>
nnoremap \| ^
nnoremap ü o<ESC>
nnoremap Ü O<ESC>

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

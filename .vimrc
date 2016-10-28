call plug#begin()
  Plug 'jiangmiao/auto-pairs'
  Plug 'scrooloose/syntastic'
  Plug 'vis'
  Plug 'vim-airline/vim-airline'
  Plug 'tpope/vim-surround'
call plug#end()

if has("gui_running")
    set guifont=Anonymous\ Pro\ Regular\ 12
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
"use pipe character in insert mode
let &t_SI .= "\<Esc>[5 q"
let &t_EI .= "\<Esc>[0 q"

let mapleader=','
map <C-Tab> gt
map <C-S-Tab> gT
nnoremap <silent> <F5> :SyntasticCheck<CR>
nnoremap <silent> <F6> :SyntasticReset<CR>
nnoremap ö ^
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

syntax on

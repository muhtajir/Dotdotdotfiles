call plug#begin()
  Plug 'townk/vim-autoclose'
  Plug 'scrooloose/syntastic'
      Plug 'vis'
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

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_pylint_exec = 'pylint2'
let g:syntastic_auto_jump = 3
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }

syntax on

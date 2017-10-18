" DON'T DELETE TRAILING WHITESPACE IN THIS FILE!

call plug#begin()
    Plug 'Soares/base16.nvim'
call plug#end()


" general settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set number
set autoindent
set linespace=3
set showcmd
set mouse=a
set ttimeout
set ttimeoutlen=10
set linebreak
set showbreak=…
set breakindent
set hidden
set hlsearch
set ignorecase
set smartcase
set relativenumber
set splitbelow
set inccommand=nosplit
set wildmode=longest,list,full
set laststatus=0

"" keybinds
let mapleader=','

" restore functionality lost by mapping ',' as leader
nnoremap - ,

" general
nnoremap Y y$

" more comfortable fold navigation on QWERTZ
nnoremap <leader>j zj
nnoremap <leader>k zk
nnoremap <Space> za
nnoremap <C-Space> zA

" buffer navigation
nnoremap <silent> <leader><Tab> :bn<CR>
nnoremap <silent> <leader><S-Tab> :bp<CR>
nnoremap <silent> <leader>x :bp<bar>sp<bar>bn<bar>bd<CR>
nnoremap <leader>b :buffer 
nnoremap <silent> <leader>B :buffers<CR>
nnoremap <leader>e :e 
nnoremap <leader>rc :vsplit $MYVIMRC<CR>

" split commands closer to i3
nnoremap <silent> <leader>hs :vsplit<CR>
nnoremap <silent> <leader>vs :split<CR>

" turn off search highlighting until next search
nnoremap <silent> ´ :nohlsearch<CR>

" system clipboard accesible by prepending leader key
nnoremap <leader>y "+y
nnoremap <leader>p "+p
nnoremap <leader>Y "+y$
nnoremap <leader>P "+P
nnoremap <leader>d "+d
nnoremap <leader>D "+D
onoremap <leader>y "+y
onoremap <leader>p "+p
onoremap <leader>Y "+y$
onoremap <leader>P "+P
onoremap <leader>d "+d
onoremap <leader>D "+D
vnoremap <leader>y "+y
vnoremap <leader>p "+p
vnoremap <leader>Y "+y$
vnoremap <leader>P "+P
nmap <leader>gr "+gr
nmap <leader>gR "+gR
vmap <leader>gr "+gr
vmap <leader>gR "+gR
omap <leader>gr "+gr
omap <leader>gR "+gR

" window navigation with alt key
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l
nnoremap <silent> <A-c> :clo<CR>
nnoremap <silent> <S-A-c> :bp<bar>sp<bar>bn<bar>bd<bar>clo<CR>

" also in terminal mode
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
tnoremap <silent> <A-c> <C-\><C-n>:clo<CR>
tnoremap <silent> <S-A-c> :bp<bar>sp<bar>bn<bar>bd<bar>clo<CR>

" use up-down bindings from my shell configuration
cnoremap <A-k> <Up>
cnoremap <A-j> <Down>

" delete word using ctrl or alt + backspace in command mode
cnoremap <A-BS> <C-W>
cnoremap <C-BS> <C-W>

" section navigation that works better with an ISO keyboard
nnoremap ( {
nnoremap ) }
onoremap ( {
onoremap ) }
nnoremap { (
nnoremap } )
onoremap { (
onoremap } )

" netrw
let g:netrw_banner = 0

" base16.nvim color settings
set termguicolors
let g:base16_transparent_background = 1
set background=dark
set cursorline
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme ocean

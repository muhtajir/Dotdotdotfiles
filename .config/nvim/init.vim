" DON'T DELETE TRAILING WHITESPACE IN THIS FILE!

call plug#begin()
    Plug 'chrisbra/SudoEdit.vim'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'dag/vim-fish'
    Plug 'dietsche/vim-lastplace'
    Plug 'dojoteef/neomake-autolint'
    Plug 'kana/vim-textobj-indent'
    Plug 'kana/vim-textobj-user'
    Plug 'kshenoy/vim-signature'
    Plug 'mtth/scratch.vim'
    Plug 'neomake/neomake'
    Plug 'raimondi/delimitmate'
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'skywind3000/asyncrun.vim'
    Plug 'thinca/vim-quickrun'
    Plug 'tmhedberg/SimpylFold'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-surround'
    " Plug 'tpope/vim-vinegar'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-scripts/ReplaceWithRegister'
    Plug 'vim-scripts/vis'
    " deoplete completions
    Plug 'Shougo/neco-syntax'
    Plug 'Soares/base16.nvim'
    Plug 'zchee/deoplete-jedi'
call plug#end()


" make nvim use bash if started from fish
if &shell =~# 'fish$'
    set shell=bash
endif

" load included matchit_plugin
runtime macros/matchit.vim


" functions
" from vimcasts.org
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    " restore previous position, before the find function if that has been run
    if exists('b:__s')
        call cursor(b:__l, b:__c)
        let @/=b:__s
        unlet b:__l
        unlet b:__c
        unlet b:__s
    else
        let @/=_s
        call cursor(l, c)
    endif
endfunction

function! <SID>FindTrailingWhitespaces()
    if !exists('b:__s')
        let b:__l = line(".")
        let b:__c = col(".")
        let b:__s=@/
    endif
    let @/='\s\+$'
    norm n
endfunction


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
set hidden
set hlsearch
set ignorecase
set smartcase
set relativenumber
set splitbelow
set inccommand=nosplit

" enable folding
set foldmethod=syntax
set foldnestmax=2

"" set what to ignore when using wildcards (mainly relevant for ctrlP)
set wildignore+=*/__pycache__/*


"" autocommands
" source the .vimrc/init.vim automatically after saving
autocmd bufwritepost init.vim source $MYVIMRC | AirlineRefresh
" close preview window after leaving insert mode
autocmd InsertLeave * silent! pclose!


"" aliases
" change working directory to current file's parent folder
cnoreabbrev <silent> here lcd %:p:h


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

" ctrlP buffer mode shortcut
nnoremap <C-B> :CtrlPBuffer<CR>

" split commands closer to i3
nnoremap <silent> <leader>hs :vsplit<CR>
nnoremap <silent> <leader>vs :split<CR>

" auto-delete trailing whitespace
nnoremap <silent> <F3> :call <SID>FindTrailingWhitespaces()<CR>
nnoremap <silent> <leader><F3> :call <SID>StripTrailingWhitespaces()<CR>

" turn off search highlighting until next search
nnoremap <silent> <F4> :nohlsearch<CR>

" run Neomake
nnoremap <silent> <F5> :Neomake<CR>

" insert blank links without entering insert mode
nnoremap ö o<ESC>k
nnoremap Ö O<ESC>j
nnoremap <leader>ö o<ESC>
nnoremap <leader>Ö O<ESC>

" system clipboard accesible by prepending leader key
nnoremap <leader>y "+y
nnoremap <leader>p "+p
nnoremap <leader>Y "+y$
nnoremap <leader>P "+P
nnoremap <leader>d "+d
nnoremap <leader>D "+D
nmap <leader>gr "+gr
nmap <leader>gR "+gR
vnoremap <leader>y "+y
vnoremap <leader>p "+p
vnoremap <leader>Y "+y$
vnoremap <leader>P "+P
vmap <leader>gr "+gr
vmap <leader>gR "+gR

" window navigation with alt key
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l
nnoremap <silent> <A-c> :clo<CR>

" also in terminal mode
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
tnoremap <silent> <A-c> <C-\><C-n>:clo<CR>

" jump to end of line in insert mode
inoremap æ <Esc>A

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

" file navigation
nnoremap <silent> <A-o> :Explore<CR>

" SudoEdit
nnoremap <leader>sw :SudoWrite<CR>

" Quickrun
nnoremap <F12> :QuickRun<CR>

" create ctags
nnoremap <silent> <C-C> :AsyncRun ctags -R .<CR>

" shortcut for pytest
nnoremap <F9> :terminal PYTHONPATH=$(pwd) pytest<CR>

"" plugin configuration
" ctrlp
" keybinds consistent with command mode
let g:ctrlp_prompt_mappings = {
    \ 'PrtSelectMove("j")':    ['<A-j>'],
    \ 'PrtSelectMove("k")':    ['<A-k>'],
    \ }

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#jedi#show_docstring = 1
call deoplete#custom#set('jedi', 'rank', 1000)

" netrw
let g:netrw_banner = 0

" neomake
let g:neomake_python_enabled_makers = ['flake8']

" neomake-autolint
let g:neomake_autolint_sign_column_always = 1
let g:neomake_autolint_events = {
    \ 'InsertLeave': {'delay': 0},
    \ 'TextChanged': {'delay': 0},
    \ 'BufWritePost': {'delay': 0},
    \ }

" scratch
let g:scratch_insert_autohide = 0

" simpylfold (what's with the name?)
let g:SimpylFold_fold_docstring = 0
let g:SimpylFold_fold_import = 0

" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" base16.nvim color settings
set termguicolors
set background=dark
set cursorline
let base16colorspace=256  " Access colors present in 256 colorspace
let g:base16_transparent_background = 1
let g:base16_airline = 1
colorscheme ocean
Base16Highlight Pmenu bg=dark1
Base16Highlight PmenuSel bg=dark3
Base16Highlight NeomakeWarningSign fg=yellow

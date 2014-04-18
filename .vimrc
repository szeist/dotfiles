"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle plugin manager
" https://github.com/gmarik/Vundle.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Install plugins with vundle
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Color scheme
Bundle 'lsdr/monokai'
" File browser
Bundle 'scrooloose/nerdtree'
" File browser tab support
Bundle 'jistr/vim-nerdtree-tabs'
" VCS support
Bundle 'vcscommand.vim'
" Multiple cursors
Bundle 'terryma/vim-multiple-cursors'
" Outline viewer
Bundle 'majutsushi/tagbar'
" Auto completion popup
Bundle 'vim-scripts/AutoComplPop'
" Fuzzy file finder
Bundle 'kien/ctrlp.vim'
" Ag searcher (needs silversearcher-ag)
Bundle 'rking/ag.vim'
" Buffer explorer
Bundle 'jlanzarotta/bufexplorer'

" Dockerfile syntax
Bundle 'ekalinin/Dockerfile.vim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable filetype plugins
filetype plugin indent on

" Auto read file changes
set autoread

" Allow use backspaces
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Turn off backups and swap files
set nobackup
set nowb
set noswapfile

" Hide abandoned buffers
set hid

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Indent settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set tab width
set shiftwidth=4
set tabstop=4

" Use spaces instead of tabs
set expandtab
set smarttab

" Auto indent
set ai
set si

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Search case settings
set ignorecase
set smartcase

" Highlight search
set hls
" Incremental search
set incsearch

" Use magic regexps
set magic

" Show matching brackets
set showmatch

" Don't redraw while running macros
set lazyredraw

" Turn of beels
set noerrorbells
set novisualbell

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Automatic commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Remember last position
" http://amix.dk/vim/vimrc.html 
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
" Remember open buffers
set viminfo^=%

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UI settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=1

" Always show tabline
set showtabline=2

" Wildmode autocompletion
set wildmenu
set wildmode=list:longest,full

" Wrap lines
set wrap

" Line numbers
set nu

" Syntax highlight
syntax on

" Always show postision
set ruler

" GUI dependent settings
if has("gui_running")
    colorscheme monokai

    " Remove toolbar
    set guioptions-=T
else
    colorscheme koehler
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Toggle tagbar
nmap <F8> :TagbarToggle<CR>

let g:multi_cursor_next_key="\<C-d>"
let g:multi_cursor_next_key="\<C-S-d>"

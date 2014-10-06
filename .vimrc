"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle plugin manager
" https://github.com/gmarik/Vundle.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Install plugins with vundle
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Color scheme
Plugin 'lsdr/monokai'
" File browser
Plugin 'scrooloose/nerdtree'
" File browser tab support
Plugin 'jistr/vim-nerdtree-tabs'
" VCS support
Plugin 'vcscommand.vim'
" Multiple cursors
Plugin 'terryma/vim-multiple-cursors'
" Outline viewer
Plugin 'majutsushi/tagbar'
" Auto completion popup
Plugin 'vim-scripts/AutoComplPop'
" Fuzzy file finder
Plugin 'kien/ctrlp.vim'
" Ag searcher (needs silversearcher-ag)
Plugin 'rking/ag.vim'
" Buffer explorer
Plugin 'jlanzarotta/bufexplorer'
" Indet detectuin plugin
Plugin 'ciaranm/detectindent'

" Dockerfile syntax
Plugin 'ekalinin/Dockerfile.vim'
" Javascript syntax
Plugin 'pangloss/vim-javascript'
" Coffescript syntax
Plugin 'kchmck/vim-coffee-script'
" SCSS syntax
Plugin 'cakebaker/scss-syntax.vim'

call vundle#end()

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

    set guifont=monospace\ 10

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

" Multi cursor
let g:multi_cursor_next_key="\<C-d>"
let g:multi_cursor_next_key="\<C-S-d>"

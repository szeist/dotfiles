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
" Workspace plugin
Plugin 'szw/vim-ctrlspace'
" Improved status/tabline
Plugin 'bling/vim-airline'
" Show VCS modifications
Plugin 'mhinz/vim-signify'
" Syntax checker
Plugin 'scrooloose/syntastic'

" Dockerfile syntax
Plugin 'ekalinin/Dockerfile.vim'
" Javascript syntax
Plugin 'pangloss/vim-javascript'
" Coffescript syntax
Plugin 'kchmck/vim-coffee-script'
" SCSS syntax
Plugin 'cakebaker/scss-syntax.vim'
" Twig syntax
Plugin 'evidens/vim-twig'

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

" Search ignore patterns
set wildignore+=.git,.hg
set wildignore+=*/tmp/*,*/temp/*,*/cache/*
set wildignore+=*.o,*.so,*.swp,*.zip
set wildignore+=node_modules,bower_components,vendor

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

" Filetype specific indents
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType json setlocal shiftwidth=2 tabstop=2 expandtab
autocmd FileType html setlocal shiftwidth=2 tabstop=2 expandtab

" Filetype specific syntax
autocmd BufRead,BufNewFile .xmobarrc setfiletype haskell

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

" Always show statusline
set laststatus=2

" GUI dependent settings
if has("gui_running")
    colorscheme monokai

    set guifont=Dejavu\ Sans\ Mono\ 10

    " Remove toolbar
    set guioptions-=T
	set guioptions-=m
	set guioptions-=L
	set guioptions-=r
	set guioptions-=e
else
    colorscheme koehler
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" CtrlP settings
let g:ctrlp_show_hidden=1

" Airline settings
let g:airline_theme='molokai'
let g:airline_exclude_preview=1

" NERDTree settings
let g:nerdtree_tabs_open_on_gui_startup=0

" Signnify settings
let g:signify_vcs_list=['git', 'hg']
let g:signify_update_on_bufenter = 1

" Sintastic settings
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_php_checkrs=['php']
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_hs_checkers = ['ghc-mod', 'hlint']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Toggle tagbar
nmap <F8> :TagbarToggle<CR>
map <Leader>n <plug>NERDTreeTabsToggle<CR>
map <Leader>t :tabnew<CR>

" Multi cursor
let g:multi_cursor_next_key="\<C-d>"
let g:multi_cursor_next_key="\<C-S-d>"

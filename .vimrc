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
Plugin 'Shougo/neocomplete'
" Fuzzy file finder
Plugin 'kien/ctrlp.vim'
" Ag searcher (needs silversearcher-ag)
Plugin 'rking/ag.vim'
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
" CTags updater
Plugin 'xolox/vim-misc'
" Automated tag generation
Plugin 'xolox/vim-easytags'
" Tmux integration
Plugin 'epeli/slimux'

" Scala plugin
Plugin 'derekwyatt/vim-scala'
" SBT integration plugin
Plugin 'ktvoelker/sbt-vim'
" C++ code completion
Plugin 'vim-scripts/OmniCppComplete'

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
set wildignore+=tmp/*,temp/*,*/cache/*,.rsync_cache
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
autocmd FileType javascript,json,html,html.twig,htmldjango.twig,yaml,scss,css,xml,python,jade,coffee,haskell setlocal shiftwidth=2 tabstop=2 expandtab

" Filetype specific syntax
autocmd BufRead,BufNewFile .xmobarrc setfiletype haskell

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType cpp,hpp set omnifunc=omni#cpp#complete#Main

" Hdevtools key bindings
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>

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

" Signify settings
let g:signify_vcs_list=['git', 'hg']
let g:signify_update_on_bufenter = 1

" Syntastic settings
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_php_checkers = ['php']
let g:syntastic_hs_checkers = ['hlint']

" Easytags settings
"set tags=./tags
let g:easytags_async=1
"let g:easytags_dynamic_files=1
let g:easytags_events = ['BufWritePost']

" Neocomplete settings
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif

" Turn off neocomplete  when multiple cursors are active
"
function! Multiple_cursors_before()
    exe 'NeoCompleteLock'
    echo 'Disabled autocomplete'
endfunction

function! Multiple_cursors_after()
    exe 'NeoCompleteUnlock'
    echo 'Enabled autocomplete'
endfunction

" Slimux settings
map <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
map <Leader>a :SlimuxShellLast<CR>
map <Leader>k :SlimuxSendKeysLast<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Toggle tagbar
nmap <F8> :TagbarToggle<CR>
map <Leader>n <plug>NERDTreeTabsToggle<CR>
map <Leader>t :tabnew<CR>

" Multi cursor
let g:multi_cursor_next_key="\<C-d>"

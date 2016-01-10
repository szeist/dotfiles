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
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Color scheme
Plugin 'lsdr/monokai'
" File browser
Plugin 'scrooloose/nerdtree'
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
" Editorconfig support
Plugin 'editorconfig/editorconfig-vim'
" Support local vimrc
Plugin 'LucHermitte/lh-vim-lib'
Plugin 'LucHermitte/local_vimrc'

" Scala plugin
Plugin 'derekwyatt/vim-scala'
" SBT integration plugin
Plugin 'ktvoelker/sbt-vim'
" C++ code completion
Plugin 'vim-scripts/OmniCppComplete'
" Octave plugin
Plugin 'jvirtanen/vim-octave'
" Typescript plugin
Plugin 'leafgarland/typescript-vim'
" Powershell syntax
Plugin 'PProvost/vim-ps1'

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
" Jade syntax
Plugin 'digitaltoad/vim-jade'

call vundle#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

filetype plugin indent on

" Auto read file changes
set autoread

" Allow use backspaces
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set nobackup
set nowb
set noswapfile

" Hide abandoned buffers
set hid

" Enable mouse in all modes
set mouse=a

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Indent settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set shiftwidth=2
set tabstop=2

" Use spaces instead of tabs
set expandtab
set smarttab

" Auto indent
set ai
set si

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set ignorecase
set smartcase

set hls
set incsearch

" Show matching brackets
set showmatch

set wildignore+=.git,.hg,.svn
set wildignore+=*/tmp,*/temp,*/.tmp,*/cache,*/.rsync_cache,*/.sass-cache
set wildignore+=*.o,*.so,*.swp,*.zip
set wildignore+=*/node_modules,*/bower_components,*/vendor

" Don't redraw while running macros
set lazyredraw

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

autocmd VimEnter * SourceLocalVimrc

autocmd FileType php,perl,python setlocal shiftwidth=4 tabstop=4 expandtab

autocmd BufRead,BufNewFile .xmobarrc setfiletype haskell

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType cpp,hpp set omnifunc=omni#cpp#complete#Main

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UI settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=2

" Always show tabline
set showtabline=2

" Wildmode autocompletion
set wildmenu
set wildmode=list:longest,full

set wrap

set nu

syntax on

set ruler

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
" Functions settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Turn off neocomplete  when multiple cursors are active
"
function! Multiple_cursors_before()
    exe 'NeoCompleteLock'
endfunction

function! Multiple_cursors_after()
    exe 'NeoCompleteUnlock'
endfunction

" Strip whitespaces (and \r \n characters)
function! Trim(input_string)
  return substitute(a:input_string, '\v^\s*(.{-})[\s\r\n]$','\1','')
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:ctrlp_show_hidden=1

let g:airline_theme='molokai'
let g:airline_exclude_preview=1

let g:signify_vcs_list=['git', 'hg', 'svn']
let g:signify_update_on_bufenter = 1

let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1
let g:syntastic_javascript_checkers = ['eslint']
" Detect local eslint
let g:syntastic_javascript_eslint_exec = Trim(system('npm-which eslint'))
let g:syntastic_php_checkers = ['php']

let g:easytags_async=1
let g:easytags_dynamic_files=1
let g:easytags_events = ['BufWritePost']

let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
let g:neocomplete#sources#omni#input_patterns = {}

let g:CtrlSpaceLoadLastWorkspaceOnStart = 1
let g:CtrlSpaceSaveWorkspaceOnSwitch = 1
let g:CtrlSpaceSaveWorkspaceOnExit = 1

let g:local_vimrc = ".vimrc_local.vim"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Toggle tagbar
nmap <F8> :TagbarToggle<CR>
map <Leader>n :NERDTreeToggle<CR>

" Multi cursor
let g:multi_cursor_next_key="\<C-d>"

" Slimux key bindings
map <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
map <Leader>a :SlimuxShellLast<CR>
map <Leader>k :SlimuxSendKeysLast<CR>

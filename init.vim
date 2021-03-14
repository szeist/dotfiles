""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
Plugin 'altercation/vim-colors-solarized'
" File browser
Plugin 'scrooloose/nerdtree'
" VCS support
Plugin 'vcscommand.vim'
" Auto completion popup
Plugin 'Shougo/deoplete.nvim'
" Fuzzy file finder
Plugin 'kien/ctrlp.vim'
" Ack searcher
Plugin 'mileszs/ack.vim'
" Indet detectuin plugin
Plugin 'ciaranm/detectindent'
" Workspace plugin
Plugin 'vim-ctrlspace/vim-ctrlspace'
" Improved status/tabline
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
" Show VCS modifications
Plugin 'mhinz/vim-signify'
" Syntax checker
Plugin 'benekastah/neomake'
" Editorconfig support
Plugin 'editorconfig/editorconfig-vim'
" Commenter
Plugin 'scrooloose/nerdcommenter'
" Notes plugin
Plugin 'vimwiki/vimwiki'

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

set wildignore+=.git,.svn
set wildignore+=*/tags
set wildignore+=*/dist
set wildignore+=*/tmp,*/temp,*/.tmp,*/cache,*/.rsync_cache,.cabal-sandbox
set wildignore+=*.o,*.so,*.swp,*.zip,*.pyc
set wildignore+=*/node_modules,*/vendor,*/__pycache__

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

autocmd FileType php,perl,python setlocal shiftwidth=4 tabstop=4 expandtab

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType cpp,hpp set omnifunc=omni#cpp#complete#Main
autocmd FileType php setlocal omnifunc=phpcomplete_extended#CompletePHP

autocmd! BufRead,BufWritePost * Neomake

autocmd BufWritePost *.py,*.rb,*.go,*.java,*.scala,*.php,*.js,*.ts,*.hs,*.R
  \ call jobstart(['ctags', '-R', '.'])

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UI settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=2

" Hide tabline
set showtabline=0

" Wildmode autocompletion
set wildmenu
set wildmode=list:longest,full

set wrap

set nu

syntax on

set ruler

let g:solarized_termcolors=256
set t_Co=256

set background=dark
colorscheme solarized

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Strip whitespaces (and \r \n characters)
function! Trim(input_string)
  return substitute(a:input_string, '\v^\s*(.{-})[\s\r\n]$','\1','')
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:deoplete#enable_at_startup = 1
call deoplete#custom#var('input_patterns', {})
call deoplete#custom#option('max_list', 20)

let g:ctrlp_show_hidden=1

let g:airline_theme='solarized'
let g:airline_exclude_preview=1

let g:signify_vcs_list=['git', 'svn']
let g:signify_update_on_bufenter = 1

" CtrlSpace configuration
let g:CtrlSpaceLoadLastWorkspaceOnStart = 1
let g:CtrlSpaceSaveWorkspaceOnSwitch = 1
let g:CtrlSpaceSaveWorkspaceOnExit = 1

" Fix neovim capture
if has('nvim')
  nmap <c-space> <nul> 
end

let g:ackprg='ag --vimgrep -S --nocolor --nogroup --column --ignore node_modules --ignore dist --ignore tags --ignore vendor --ignore .git'

" Vimwiki settings
let g:vimwiki_path = '~/Documents/vimwiki/'
let g:vimwiki_syntax = 'markdown'
let g:vimwiki_ext = '.md'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <Leader>n :NERDTreeToggle<CR>


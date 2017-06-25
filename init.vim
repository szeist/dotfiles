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
Plugin 'altercation/vim-colors-solarized'
" File browser
Plugin 'scrooloose/nerdtree'
" VCS support
Plugin 'vcscommand.vim'
" Multiple cursors
Plugin 'terryma/vim-multiple-cursors'
" Auto completion popup
Plugin 'Shougo/deoplete.nvim'
" Fuzzy file finder
Plugin 'kien/ctrlp.vim'
" Ag searcher (needs silversearcher-ag)
Plugin 'rking/ag.vim'
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
" CTags updater
Plugin 'xolox/vim-misc'
" Automated tag generation
" Plugin 'xolox/vim-easytags'
" Tmux integration
Plugin 'epeli/slimux'
" Editorconfig support
Plugin 'editorconfig/editorconfig-vim'
" Support local vimrc
Plugin 'LucHermitte/lh-vim-lib'
Plugin 'LucHermitte/local_vimrc'
" Commenter
Plugin 'scrooloose/nerdcommenter'

" Scala plugin
 Plugin 'derekwyatt/vim-scala'
" C++ code completion
" Plugin 'vim-scripts/OmniCppComplete'
" Octave plugin
" Plugin 'jvirtanen/vim-octave'
" Typescript plugin
Plugin 'leafgarland/typescript-vim'
" Powershell syntax
" Plugin 'PProvost/vim-ps1'

" R plugins
Plugin 'jalvesaq/Nvim-R'
Plugin 'lyuts/vim-rtags'

" Haskell plugins
"Plugin 'eagletmt/ghcmod-vim'
"Plugin 'eagletmt/neco-ghc'
"Plugin 'neovimhaskell/haskell-vim'

Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/unite.vim'
Plugin 'm2mdas/phpcomplete-extended'

" PHP refactor tool
" Plugin 'adoy/vim-php-refactoring-toolbox'

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
" NodeJS tools
Plugin 'moll/vim-node'

" Markdown preview
" apt-get install markdown
Plugin 'vim-markdown-preview'

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
set wildignore+=*/tags
set wildignore+=*/dist
set wildignore+=*/tmp,*/temp,*/.tmp,*/cache,*/.rsync_cache,*/.sass-cache,.cabal-sandbox
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
"autocmd FileType cpp,hpp set omnifunc=omni#cpp#complete#Main
"autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd  FileType  php setlocal omnifunc=phpcomplete_extended#CompletePHP

autocmd! BufRead,BufWritePost * Neomake

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

if has("gui_running")
  " Remove toolbar
  set guioptions-=T
  set guioptions-=m
  set guioptions-=L
  set guioptions-=r
  set guioptions-=e
endif

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
let g:deoplete#omni#input_patterns = {}
" let g:deoplete#omni#input_patterns.php = '\w{3,}|[^. \t]->\w*|\w{3,}::\w*'
let g:deoplete#max_list = 20

let g:ctrlp_show_hidden=1

let g:airline_theme='solarized'
let g:airline_exclude_preview=1

let g:signify_vcs_list=['git', 'hg', 'svn']
let g:signify_update_on_bufenter = 1

" let g:syntastic_check_on_open=1
" let g:syntastic_enable_signs=1
" let g:syntastic_javascript_checkers = ['eslint']
" Detect local eslint
" let g:syntastic_javascript_eslint_exec = Trim(system('npm-which eslint'))

" CtrlSpace configuration
let g:CtrlSpaceLoadLastWorkspaceOnStart = 1
let g:CtrlSpaceSaveWorkspaceOnSwitch = 1
let g:CtrlSpaceSaveWorkspaceOnExit = 1

" Fix neovim capture
if has('nvim')
  nmap <c-space> <nul> 
end


"let g:haskell_enable_quantification = 1 
"let g:haskell_enable_recursivedo = 1
"let g:haskell_enable_arrowsyntax = 1
"let g:haskell_enable_pattern_synonyms = 1
"let g:haskell_enable_typeroles = 1
"let g:haskell_enable_static_pointers = 1
"let g:haskell_backpack = 1

"let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

"let g:haskellmode_completion_ghc = 1

"let g:haskell_tabular = 1

let g:ag_prg='ag -S --nocolor --nogroup --column --ignore node_modules --ignore dist --ignore tags'

let g:local_vimrc = ".vimrc_local.vim"

let vim_markdown_preview_toggle=1
let vim_markdown_preview_hotkey='<C-m>'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <Leader>n :NERDTreeToggle<CR>

let g:multi_cursor_next_key="\<C-d>"

" Slimux key bindings
map <Leader>a :SlimuxShellLast<CR>
map <Leader>s :SlimuxREPLSendSelection<CR>
map <Leader>l :SlimuxREPLSendLine<CR>
map <Leader>k :SlimuxSendKeysLast<CR>

map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

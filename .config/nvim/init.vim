""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle plugin manager
" https://github.com/gmarik/Vundle.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
filetype off

call plug#begin('~/.vim/plugged')

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Color scheme
Plug 'altercation/vim-colors-solarized'
" File browser
Plug 'scrooloose/nerdtree'
" Indet detectuin plugin
Plug 'ciaranm/detectindent'
" Improved status/tabline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Show VCS modifications
Plug 'mhinz/vim-signify'
" Syntax checkers
Plug 'benekastah/neomake'
Plug 'dense-analysis/ale'
" Editorconfig support
Plug 'editorconfig/editorconfig-vim'
" Commenter
Plug 'scrooloose/nerdcommenter'
" Notes plugin
Plug 'vimwiki/vimwiki'
" Telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter'

" Python
Plug 'neoclide/coc.nvim', { 'branch': 'master', 'do': 'yarn install --frozen-lockfile' }
Plug 'pappasam/coc-jedi', { 'do': 'yarn install --frozen-lockfile && yarn build', 'branch': 'main' }

" Rust
Plug 'rust-lang/rust.vim'

call plug#end()


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

" Don't redraw while running macros
set lazyredraw

set noerrorbells
set novisualbell

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" UI settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=2

" Hide tabline
set showtabline=0

set wrap

set nu

syntax on

set ruler

let g:solarized_termcolors=256
set t_Co=256

set background=dark
colorscheme solarized

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:airline_theme='solarized'
let g:airline_exclude_preview=1

let g:signify_vcs_list=['git']
let g:signify_update_on_bufenter = 1

" Vimwiki settings
let g:vimwiki_path = '~/Documents/vimwiki/'
let g:vimwiki_syntax = 'markdown'
let g:vimwiki_ext = '.md'

lua << EOF
require("telescope").setup { 
  defaults = {
    file_ignore_patterns = {'node_modules', 'vendor'} 
  } 
}
EOF

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

map <Leader>n :NERDTreeToggle<CR>

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>


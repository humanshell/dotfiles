" be iMproved
set nocompatible

" Vundle required setup/init
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" My Bundles:
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..
"
" original repos on github
" EXAMPLE: Bundle 'tpope/vim-rails.git'
Bundle 'tpope/vim-fugitive.git'
Bundle 'tpope/vim-markdown.git'
Bundle 'tpope/vim-haml.git'
Bundle 'tpope/vim-rails.git'
Bundle 'tpope/vim-bundler.git'
Bundle 'tpope/vim-endwise.git'
Bundle 'tpope/vim-surround.git'
Bundle 'scrooloose/nerdtree.git'
Bundle 'mileszs/ack.vim.git'
Bundle 'panozzaj/vim-autocorrect.git'
Bundle 'Townk/vim-autoclose.git'
Bundle 'kien/ctrlp.vim.git'
Bundle 'honza/vim-snippets.git'
"
" vim-scripts repos
" EXAMPLE: Bundle 'L9'
"
" non github repos
" EXAMPLE: 'git://git.wincent.com/command-t.git'

" general setup
filetype plugin indent on     " required!
syntax enable
set background=dark
let mapleader = ","
let g:mapleader = ","
set modelines=0
set history=1000
set nobackup
set nowritebackup
set noswapfile
set autoread
set showcmd

" ui setup
set encoding=utf-8
set cursorline
set scrolloff=3
set number
set autoindent
set smartindent
set showmode
set hidden
set wildmenu
set wildmode=list:longest
set ttyfast
set ruler
set backspace=indent,eol,start
set laststatus=2

" status line setup
set statusline=%f                               " path
set statusline+=%m%r%h%w                        " flags
set statusline+=\ %{fugitive#statusline()}      " git branch
set statusline+=\ [%{strlen(&fenc)?&fenc:&enc}] " encoding
set statusline+=\ [line\ %l\/%L\ %p%%]          " line x of y
" color
hi StatusLine ctermfg=Black ctermbg=White
au InsertEnter * hi StatusLine ctermbg=DarkBlue
au InsertLeave * hi StatusLine ctermfg=Black ctermbg=White

" text and formatting setup
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set nowrap
set textwidth=79
set formatoptions=n
set t_Co=256
if exists("+colorcolumn")
  set colorcolumn=80
endif

" bind ctrl-l to hash rocket
imap <C-l> <Space>=><Space>

" window height setup
set winheight=999
set previewheight=50
au! BufEnter ?* call PreviewHeightWorkAround()
func PreviewHeightWorkAround()
  if &previewwindow
    exec 'setlocal winheight='.&previewheight
  endif
endfunc

" moving and searching setup
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
" turn search highlight off
nnoremap <leader><space> :noh<cr>
" search (forwards)
nmap <space> /
" search (backwards)
map <c-space> ?
" Center screen when scrolling search results
nmap n nzz
nmap N Nzz

" map ESC
imap jj <ESC>

" ACK
set grepprg=ack
nnoremap <leader>a :Ack

" turn off arrow keys
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
nnoremap <Down> :echoe "Use j"<CR>"
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

" move lines up and down
map <C-J> :m +1 <CR>
map <C-K> :m -2 <CR>

" switch between buffers
noremap <tab> :bn<CR>
noremap <S-tab> :bp<CR>

" close buffer
nmap <leader>d :bd<CR>

" close all buffers
nmap <leader>D :bufdo bd<CR>

" switch between last two buffers
nnoremap <leader><leader> <c-^>

" Saving and exit
nmap <leader>q :q<CR>
nmap <leader>w :w<CR>

" find files with ctrlp
map <leader>f :CtrlP

" nerdtree setup
map <leader>p :NERDTreeToggle<cr>
let NERDTreeShowBookmarks = 0
let NERDChristmasTree = 1
let NERDTreeWinPos = "left"
let NERDTreeHijackNetrw = 1
let NERDTreeQuitOnOpen = 1
let NERDTreeWinSize = 50 
let NERDTreeChDirMode = 2
let NERDTreeDirArrows = 1

" autoClose
let g:AutoClosePairs = {'(': ')', '{': '}', '[': ']', '"': '"', "'": "'", '#{': '}'}
let g:AutoCloseProtectedRegions = ["Character"]

" vim-rails
map <Leader>m :Rmodel
map <Leader>v :Rview

" gui setup
if has("gui_running")
  set guioptions-=T " no toolbar set guioptions-=m " no menus
  set guioptions-=r " no scrollbar on the right
  set guioptions-=R " no scrollbar on the right
  set guioptions-=l " no scrollbar on the left
  set guioptions-=b " no scrollbar on the bottom
  set guioptions=aiA 
  set mouse=v
  "set guifont=Monaco:h12 "<- Maybe a good idea when using mac
endif
"set guifont=Monaco:h12

let my_home = expand("$HOME/")

if filereadable(my_home . '.vim/bundle/vim-autocorrect/autocorrect.vim')
  source ~/.vim/bundle/vim-autocorrect/autocorrect.vim
endif

" When vimrc is edited, automatically reload it
autocmd! bufwritepost $MYVIMRC source %


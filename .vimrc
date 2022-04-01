syntax on
set nocompatible   " Disable vi compatibilty
set encoding=utf-8 " Use UTF-8
set number relativenumber
set numberwidth=5
"set cursorline
"set cursorcolumn
set autoindent
set nowrap
set nobackup
set noswapfile
set belloff=all
set colorcolumn=120
set noshowmode
"set mouse=a        " Enable mouse mode
set autoindent     " Enable autoindent
set showmatch      " Show matching brackets
set nomodeline     " Disable as a security precaution
set wildmenu       " Enable wildmenu
set scrolloff=8    " Scroll offset
set history=1000   " More history
set laststatus=2   " Always show status line


	"---------Search--------"
set ignorecase     " Do case insensitive matching
set incsearch      " Show partial matches for a search phrase
set nohlsearch     " clear highlights after search



	"---------Tab----------"
set tabstop=4      " Tab size
set shiftwidth=4   " Indentation size
set softtabstop=4  " Tabs/Spaces interop
set expandtab      " Expands tab to spaces
set smarttab       " Better tabs


" Plugins {{{
call plug#begin('~/.vim/plugged')

Plug 'morhetz/gruvbox'
Plug 'itchyny/lightline.vim'
Plug 'ap/vim-css-color'
Plug 'preservim/nerdtree'


call plug#end()
"}}}

" ColorScheme and backgrounds {{{
set bg=dark
colorscheme gruvbox
hi Normal guibg=NONE ctermbg=NONE
"}}}

" NRDEtree Settings {{{
" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>
"}}}

set nocompatible
filetype off

filetype plugin indent on

if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
		\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
	" Utility / Interface
	Plug 'preservim/nerdcommenter'
	Plug 'preservim/nerdtree'
	Plug 'Xuyuanp/nerdtree-git-plugin'
	Plug 'ryanoasis/vim-devicons'
	Plug 'airblade/vim-gitgutter'
	Plug 'itchyny/lightline.vim'
	" Programming support
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	Plug 'vim-syntastic/syntastic'
	Plug 'sheerun/vim-polyglot'
	Plug 'majutsushi/tagbar'
	Plug 'universal-ctags/ctags'
	" Syntax support
	Plug 'cespare/vim-toml'
	" Rust support
	Plug 'rust-lang/rust.vim'
	" Theme
	Plug 'joshdick/onedark.vim'
call plug#end()

" Color
syntax on
set termguicolors
let g:lightline = { 'colorscheme': 'onedark' }
let s:green  = { "gui": '#229977', "cterm": "35",  "cterm16": "2" }
let s:blue   = { 'gui': '#61AFEF', 'cterm': '39',  'cterm16': '4' }
let s:cyan   = { 'gui': '#56B6C2', 'cterm': '38',  'cterm16': '6' }
let s:purple = { 'gui': '#C678DD', 'cterm': '170', 'cterm16': '5' }
let s:white  = { 'gui': '#ABB2BF', 'cterm': '145', 'cterm16': '7' }
if (has('autocmd'))
	augroup colorextend
		autocmd!
		autocmd ColorScheme * call onedark#extend_highlight('Comment',        { 'fg': s:green  })
		autocmd ColorScheme * call onedark#extend_highlight('SpecialComment', { 'fg': s:green  })
		autocmd ColorScheme * call onedark#extend_highlight('Include',        { 'fg': s:white  })
		autocmd ColorScheme * call onedark#extend_highlight('Identifier',     { 'fg': s:cyan   })
		autocmd ColorScheme * call onedark#extend_highlight('Keyword',        { 'fg': s:purple })
		autocmd ColorScheme * call onedark#extend_highlight('Macro',          { 'fg': s:blue   })
		autocmd ColorScheme * call onedark#extend_highlight('StorageClass',   { 'fg': s:purple })
		autocmd ColorScheme * call onedark#extend_highlight('Type',           { 'fg': s:cyan   })
	augroup END
endif
colorscheme onedark

" Transparency
hi Normal guibg=NONE ctermbg=NONE

" Indent
set autoindent
set noexpandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Search
set hlsearch
set incsearch
set smartcase
set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case

" Text Rendering
set encoding=utf8
set nowrap

" User Interface
set laststatus=2
set ruler
set cursorline
set number
set relativenumber

" Miscellaneous
set backspace=indent,eol,start
set history=1000
set nobackup
set noswapfile
set mouse=n

" Autocommands
autocmd BufWritePre * %s/\s\+$//e

" Remaps
noremap <up>    <C-w><up>
noremap <down>  <C-w><down>
noremap <left>  <C-w><left>
noremap <right> <C-w><right>
map  <C-\> :NERDTreeToggle<CR>
nmap <C-g> <Plug>(coc-definition)
"nmap <silent> gd <Plug>(coc-definition)
"nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation)
"nmap <silent> gr <Plug>(coc-references)
nnoremap <C-]> :call NERDComment(0,"toggle")<CR>
vnoremap <C-]> :call NERDComment(0,"toggle")<CR>

" COC setting
let g:coc_global_extensions = [
	\ 'coc-markdownlint',
	\ 'coc-python',
	\ 'coc-rust-analyzer',
\ ]

" Rust setting
let g:rustfmt_autosave = 1

" Syntastic setting
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 0
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

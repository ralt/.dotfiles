set nocompatible

" Use pathogen to easily modify the runtime path to include all
" plugins under the ~/.vim/bundle directory
call pathogen#helptags()
call pathogen#infect("~/.vim/bundle")
filetype off " Avoid pathogen's bug

" change the mapleader from \ to ,
let mapleader=","

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

set hidden
set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start
                  " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
set number        " always show line numbers
set shiftwidth=4  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                  "    case-sensitive otherwise
set smarttab      " insert tabs on the start of a line according to
                  "    shiftwidth, not tabstop
set expandtab
set hlsearch      " highlight search terms
set incsearch     " show search matches as you type
set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class
set title                " change the terminal's title
set visualbell           " don't beep
set noerrorbells         " don't beep
set nobackup
set noswapfile
filetype plugin indent on
if &t_Co > 2 || has("gui_running")
  " switch syntax highlighting on, when the terminal has colors
  syntax on
endif
set bg=dark
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.
autocmd filetype html,xml set listchars-=tab:>.
set pastetoggle=<F2>
" Enable mouse
set mouse=a
" Disable arrow keys to move around
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
nmap <silent> ,/ :nohlsearch<CR>
set encoding=utf-8

" Toggle between relative and absolute numbers
function! g:ToggleNuMode()
  if(&rnu == 1)
    set nu
  else
    set rnu
  endif
endfunc
nnoremap <C-L> :call g:ToggleNuMode()<cr>

" Specific filetypes for color syntaxing
au BufNewFile,BufRead *.md set filetype=markdown
" Drupal specific settings
au BufNewFile,BufRead *.module set filetype=php
au BufNewFile,BufRead *.module set shiftwidth=2
au BufNewFile,BufRead *.admin.inc set filetype=php
au BufNewFile,BufRead *.admin.inc set shiftwidth=2
au BufNewFile,BufRead *.json set filetype=javascript

" Always keep the cursor at the middle
"set scrolloff=1000
set colorcolumn=0
set foldmethod=indent

" zencoding configurations
let g:user_zen_settings = {
            \ 'indentation': '    ',
\}
let g:user_zen_expandabbr_key = '<c-e>'


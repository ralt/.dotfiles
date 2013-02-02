set nocompatible

" Use pathogen to easily modify the runtime path to include all
" plugins under the ~/.vim/bundle directory
call pathogen#helptags()
call pathogen#infect("~/.vim/bundle")
filetype off " Avoid pathogen's bug

colorscheme solarized

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

"" Set bundles
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

Bundle 'vim-scripts/LycosaExplorer'
Bundle 'scrooloose/nerdtree'
Bundle 'Ralt/psettings'
Bundle 'ervandew/supertab'
Bundle 'vim-scripts/taglist'
Bundle 'altercation/vim-colors-solarized'
Bundle 'mattn/zencoding-vim'
Bundle 'jelera/vim-javascript-syntax'

"necessary for so many stuff
filetype plugin indent on
if &t_Co > 2 || has("gui_running")
  " switch syntax highlighting on, when the terminal has colors
  syntax on
endif

"set background
set bg=dark

"show trailing spaces
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.
autocmd filetype html,xml set listchars-=tab:>.

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
au BufNewFile,BufRead *.install set filetype=php
au BufNewFile,BufRead *.install set shiftwidth=2
au BufNewFile,BufRead *.test set filetype=php
au BufNewFile,BufRead *.test set shiftwidth=2
au BufNewFile,BufRead *.inc set filetype=php
au BufNewFile,BufRead *.inc set shiftwidth=2
au BufNewFile,BufRead *.json set filetype=javascript
au BufReadPost *.module,*.install,*.theme set syntax=php

" everything folded when getting in a file
set foldmethod=indent

" zencoding configurations
let g:user_zen_settings = {
            \ 'indentation': '    ',
\}
let g:user_zen_expandabbr_key = '<c-e>'

" use the right compiler
au FileType c set makeprg=gcc\ %
au FileType cpp set makeprg=g++\ %

" really nice red line at column 80
set colorcolumn=80

" taglist settings
let g:Tlist_Ctags_Winwidth = 0
nnoremap <Leader>l :TlistToggle<CR>

"auto open taglist
"let g:Tlist_Auto_Open = 1

" where to looks for tags: recursively up to $HOME
set tags=./tags,tags;$HOME

"php complete
"inoremap ² <C-x><C-o>

" Close taglist or nerdtree if it's the only left
fun! NoExistingBuffersLeft()
    if tabpagenr("$") == 1 && winnr("$") == 1
        if bufname(winbufnr(1)) == "__Tag_List__"
            quit
        endif
    endif
endfun

au WinEnter * call NoExistingBuffersLeft()

" syntastic conf to use phpcs correctly
let g:syntastic_phpcs_conf=" --standard=Drupal --extensions=php,module,inc,install,test,profile,theme"

" NERDTree
let NERDTreeQuitOnOpen = 1
nnoremap <Leader>n :NERDTreeToggle<CR>

" jk or kj in insert mode exit the insert mode
imap jk <C-[>
imap kj <C-[>

" noexpandtab for makefiles and gitmodules
au BufNewFile,BufRead make set noexpandtab
au BufNewFile,BufRead .gitmodules set noexpandtab

" Matching HTML tags
runtime macros/matchit.vim

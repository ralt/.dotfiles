set nocompatible

" Use pathogen to easily modify the runtime path to include all
" plugins under the ~/.vim/bundle directory
call pathogen#helptags()
call pathogen#infect("~/.vim/bundle")
filetype off " Avoid pathogen's bug

se t_Co=16

" change the mapleader from \ to ,
let mapleader=","

set hidden
set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start
                  " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
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

" Hybrid line number mode
"set relativenumber
"set number

"" Set bundles
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

Bundle 'Ralt/psettings'
Bundle 'ervandew/supertab'
Bundle 'mattn/zencoding-vim'
Bundle 'jelera/vim-javascript-syntax'
Bundle 'vim-scripts/slimv.vim'
Bundle 'tpope/vim-commentary'
Bundle 'flazz/vim-colorschemes'
Bundle 'bling/vim-airline'
Bundle 'mileszs/ack.vim'
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'majutsushi/tagbar'
Bundle 'jnwhiteh/vim-golang'
Bundle 'ap/vim-css-color'
Bundle 'junegunn/goyo.vim'
Bundle 'tpope/vim-vinegar'
Bundle 'tsaleh/vim-matchit'
Bundle 'justinmk/vim-sneak'

"necessary for so many stuff
filetype plugin indent on
if &t_Co > 2 || has("gui_running")
  " switch syntax highlighting on, when the terminal has colors
  syntax on
  set go-=m
  set go-=T
  set go-=r
  set go-=L
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
"map <up> <nop>
"map <down> <nop>
"map <left> <nop>
"map <right> <nop>
" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
nmap <silent> ,/ :nohlsearch<CR>
set encoding=utf-8

" everything folded when getting in a file
set foldmethod=indent

" zencoding configurations
let g:user_zen_settings = {
            \ 'indentation': '    ',
\}
let g:user_zen_expandabbr_key = '<c-e>'

" svn
au FileType svn set wrap
au FileType svn set tw=72

" really nice red line at column 80
set colorcolumn=80

nnoremap <Leader>l :TagbarToggle<CR>

" where to looks for tags: recursively up to $HOME
set tags=./tags,tags;$HOME
"
" Close taglist or nerdtree if it's the only left
fun! NoExistingBuffersLeft()
    if tabpagenr("$") == 1 && winnr("$") == 1
        if bufname(winbufnr(1)) == "__Tag_List__"
            quit
        endif
    endif
endfun

fun! KernelIndent()
    execute "%!gindent -nbad -bap -nbc -bbo -hnl -br -brs -c33 -cd33 -ncdb -ce -ci4 -cli0 -d0 -di1 -nfc1 -i8 -ip0 -l80 -lp -npcs -nprs -npsl -sai -saf -saw -ncs -nsc -sob -nfca -cp33 -ss -ts8 -il1"
endfun

" C stuff
autocmd filetype c set tabstop=8
autocmd filetype c set noexpandtab
autocmd filetype c set sw=8

au WinEnter * call NoExistingBuffersLeft()

" NERDTree
let NERDTreeQuitOnOpen = 1
nnoremap <Leader>n :NERDTreeToggle<CR>

" jk or kj in insert mode exit the insert mode
imap jk <C-[>
imap kj <C-[>

" noexpandtab for makefiles and gitmodules
au BufNewFile,BufRead make set noexpandtab
au BufNewFile,BufRead .gitmodules set noexpandtab
autocmd filetype mail set noexpandtab

" Matching HTML tags
runtime macros/matchit.vim

set guifont=Ubuntu\ Mono\ 13

set noerrorbells visualbell t_vb=
if has('autocmd')
  autocmd GUIEnter * set visualbell t_vb=
endif

set laststatus=2

set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

" show indented wrapped lines
set showbreak=-------->

set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows

set nopaste

" Use pman for PHP keywords using K
autocmd FileType php set keywordprg=pman

fun! Fab( arg )
    execute '!. ~/.cddeploy ' . a:arg
endfunc
command! -nargs=1 Fab call Fab(<f-args>)

colorscheme solarized

nnoremap <Leader>p :<C-u>CtrlP<CR>
nnoremap <Leader>b :<C-u>CtrlPBuffer<CR>
nnoremap <Leader>r :<C-u>CtrlPMRU<CR>

set nornu
set nonu

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

autocmd FileType go autocmd BufWritePre <buffer> Fmt

inoremap {      {}<Left>
inoremap {<CR>  {<CR>}<Esc>O
inoremap {{     {
inoremap {}     {}

let g:ctrlp_working_path_mode = ''

nnoremap <leader>. :CtrlPTag<cr>

nnoremap <leader>g :Gstatus<cr>

nnoremap <CR> :noh<CR><CR>

nnoremap <Leader>zz :let &scrolloff=999-&scrolloff<CR>

command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis | wincmd p | diffthis

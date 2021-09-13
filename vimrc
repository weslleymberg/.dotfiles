"---- General Settings ----
set nocompatible "disable VI compatibility
filetype indent on "enables filetype-specific indent files
set wildmenu "enables a graphical menu for autocomplete
set showtabline=0
colorscheme vacme
filetype plugin on
set path+=**

"---- Line Numbers ----
set number "enables line number
set relativenumber "enables relative number
set numberwidth=1 "set how much columns the numbers occupy

"---- Text Options ----
syntax off "disables syntax highlight
set nomodeline "disables file specific config
set encoding=utf-8 "set encoding to utf-8
set showmatch "highlight matching () [] {}
set wrap "wraps the text at the end of the screen
set list
set listchars=trail:.,tab:▸\ 

"---- Search Options ----
set incsearch "search as characters are entered
set nohlsearch "explicitly tells vim to not highlight matches
set ignorecase "case insensitive search
set smartcase "enables case sensitivity if the sentence contains uppercase

"---- Tab to Spaces config ----
autocmd FileType go set noexpandtab
autocmd FileType * set expandtab
set tabstop=4 "number of visual spaces per tab
set softtabstop=4 "number of visual spaces per tab when editing
set shiftwidth=4 "number os spaces to use with (auto)ident

"---- Disable Arrow Keys ----
"PS.: noremap works only in normal, visual and operator modes
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

"---- Splitting options ----
set splitright "split to the ritht
set splitbelow "split below

"---- File Browsing ----
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

"---- Personal Mappings ----
"remaps enter key to insert a blank line above the cursor
nnoremap <CR> O<Esc>j
"use 'jj' as escape key
inoremap jj <ESC>
command! MakeTags !ctags -R "need to install ctags


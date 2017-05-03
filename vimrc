"---- Enables Pathogen ----
execute pathogen#infect()

"---- General Settings ----
set nocompatible "disable VI compatibility
filetype indent on "enables filetype-specific indent files
set wildmenu "enables a graphical menu for autocomplete

"---- Color scheme ----
colorscheme evening

"---- Line Numbers ----
set number "enables line number
set relativenumber "enables relative number
set numberwidth=1 "set how much columns the numbers occupy

"---- Text Options ----
syntax enable "enables syntax highlighting
set nomodeline "disables file specific config
set encoding=utf-8 "set encoding to utf-8
set showmatch "highlight matching () [] {}
set wrap "wraps the text at the end of the screen

"---- Search Options ----
set incsearch "search as characters are entered
set nohlsearch "explicitly tells vim to not highlight matches
set ignorecase "case insensitive search
set smartcase "enables case sensitivity if the sentence contains uppercase

"---- Tab to Spaces config ----
set expandtab "transform tab in spaces
set tabstop=4 "number of visual spaces per tab
set softtabstop=4 "number of visual spaces per tab when editing
set shiftwidth=4 "number os spaces to use with (auto)ident

"---- Personal Mappings ----
"remaps enter key to insert a blank line above the cursor
nnoremap <CR> O<Esc>j 
"use 'jj' as escape key
inoremap jj <ESC>

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

"---- Show trailing spaces ----
set list
set listchars=trail:.


"---- General Settings ----
set nocompatible "disable VI compatibility
filetype indent on "enables filetype-specific indent files
set wildmenu "enables a graphical menu for autocomplete
colorscheme morning
set showtabline=0

"---- Line Numbers ----
set number "enables line number
set relativenumber "enables relative number
set numberwidth=1 "set how much columns the numbers occupy

"---- Text Options ----
syntax off "disables syntax highlighting
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

"---- Splitting options ----
set splitright "split to the ritht
set splitbelow "split below

"---- Personal commands ----
function s:OpenProject()
    execute "tcd" input("Project path: ", "", "file")
    execute "edit" input("Open project file: ", "", "file")
endfunction

command -nargs=0 Project call s:OpenProject()

function s:OpenScheme()
    vnew
    let t:active_repl = termopen("scheme", {'rpc':v:false})
endfunction

command -nargs=0 Scheme call s:OpenScheme() | normal "<C-w>w"

function s:SendToRepl(content)
    if !exists("t:active_repl")
        echo " Error - No REPL found!"
    else
        call chansend(t:active_repl, a:content . "\n")
    endif
endfunction

command -nargs=1 SendToREPL call s:SendToRepl(<args>)

nnoremap <C-c><C-c> ya(:SendToREPL @"<CR>

function s:Eval()
    let user_input = input("Eval: ")
    call s:SendToRepl(user_input)
endfunction

command -nargs=0 Eval call s:Eval()

nnoremap <C-c><C-e> :Eval<CR>

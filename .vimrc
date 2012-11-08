" -- reset distro made changes
set nocompatible

" ==============================================================================
" -- vim general
" ==============================================================================
set hidden                                 " hide buffers instead of closing them
set nowrap                                 " don't wrap lines
set tabstop=2                              " a tab is two spaces
set expandtab                              " use spaces instead of tabs
set autoindent                             " always set autoindenting on
set softtabstop=2                          " let backspace delete indent
set copyindent                             " copy the previous indentation on autoindenting
set shiftwidth=2                           " number of spaces to use for autoindenting
set shiftround                             " use multiple of shiftwidth when indenting with '<' and '>'
set backspace=indent,eol,start             " allow backspacing over everything in insert mode
set number                                 " always show line numbers
set showmatch                              " set show matching parenthesis
set ignorecase                             " ignore case when searching
set smartcase                              " ignore case if search pattern is all lowercase, case-sensitive otherwise
set smarttab                               " insert tabs on the start of a line according to shiftwidth, not tabstop
set hlsearch                               " highlight search terms
set incsearch                              " show search matches as you type
set nobackup                               " dont create any backup files
set noswapfile                             " dont create a swap file
set history=1000                           " remember more commands and search history
set undolevels=1000                        " use many muchos levels of undo
set wildignore=*.swp,*.bak,*.pyc,*.class   " ignore these when autocompleting files
set wildmenu
set wildmode=list:longest,full
set title                                  " change the terminal's title
set t_vb=                                  " to blink when there's an error: set visualbell
set noerrorbells                           " don't beep
set listchars=tab:>-,eol:¬
" set list                                   " show eol character
set scrolljump=5                           " lines to jump when cursor leaves page
set scrolloff=3                            " always keep this many lines under cursor
set virtualedit=all                        " allow cursor to move beyond last char in a line
filetype plugin indent on                  " detect filetype and load plugins and indents
syntax on                                  " syntax highlighting on

" -- colors
set t_Co=256
set t_AB=[48;5;%dm
set t_AF=[38;5;%dm
let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
color blackdust 
highlight NonText cterm=bold ctermfg=0 guifg=gray
highlight SpecialKey cterm=bold ctermfg=0 guifg=gray
highlight LineNr cterm=bold ctermfg=0 guifg=gray


" ==============================================================================
" -- UltiSnips
" ==============================================================================
let g:UltiSnipsSnippetDirectories=["UltiSnips", "snippets"]


" ==============================================================================
" -- OmniComplete Tags
" ==============================================================================
" configure tags - add additional tags here or comment out not-used ones
set tags+=~/.vim/tags/cpp
" build tags of your own project with Ctrl-F12
map <C-F12> :!ctags -R --sort=yes --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" OmniCppComplete
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
" automatically open and close the popup menu / preview window
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview

" ==============================================================================
" -- hotkeys
" ==============================================================================
" make unneccesary spaces and tabs visible
nmap <silent> <leader>l :set list!<CR>
" hotkey to paste insert mode
set pastetoggle=<F2>
" remap : to ;
nnoremap ; :
" easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" easy tab navigation
" Alt-n
nnoremap n :tabnew<CR>
" Alt-q
nnoremap q :tabclose<CR>
nnoremap <F7> :tabprevious<CR>
nnoremap <F8> :tabnext<CR>
inoremap <F7> <Esc>:tabprevious<CR>
inoremap <F8> <Esc>:tabnext<CR>
" clear highlighted word
nnoremap <silent> <leader>, :nohlsearch<CR>




" ------------------------------------------------------------------------------
if has('autocmd')
  autocmd filetype c,cpp call DoCCommands()
  autocmd! BufNewFile,BufRead *.pde setlocal ft=arduino
  autocmd! BufNewFile,BufRead *.ino setlocal ft=arduino
endif

" ==============================================================================
" C, C++
" ==============================================================================
function DoCCommands()
  " auto wrap lines after 80 characters
  set textwidth=80
  " mark right border
  set colorcolumn=81
  " highlight lines longer than 80 chars
  autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
  autocmd BufEnter * match OverLength /\%81v.*/
  " delete unneccesary whitespaces
  autocmd BufWritePre *.cpp :%s/\s\+$//e
  autocmd BufWritePre *.h :%s/\s\+$//e
  " make special characters visible
  set listchars=nbsp:#,tab:▸-,extends:»,precedes:«,trail:▸,eol:¬
  " set list
endfunction

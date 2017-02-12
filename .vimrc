" vim general {{{1
" ==============================================================================
set nocompatible
filetype off

runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()

set hidden                                 " dont close buffers, hide them

set expandtab                              " use spaces instead of tabs
set autoindent                             " always set autoindenting on
set copyindent                             " copy the previous indentation on autoindenting
set shiftwidth=2                           " number of spaces to use for autoindenting
set softtabstop=2                          " let backspace delete indent
set shiftround                             " use multiple of shiftwidth when indenting with '<' and '>'

set backspace=indent,eol,start             " allow backspacing over everything in insert mode
set virtualedit=all                        " allow cursor to move beyond last char in a line

set ignorecase                             " ignore case when searching
set smartcase                              " ignore case if search pattern is all lowercase, case-sensitive otherwise

set nostartofline                          " keep horizontal cursor pos when scrollen vertically
set smarttab                               " insert tabs on the start of a line according to shiftwidth, not tabstop

set nobackup                               " dont create any backup files
set noswapfile                             " dont create a swap file

set wildmenu                               " enable wildmenu (menu when auto completing in a command)
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.obj,build-nano*,tags   " ignore these when autocompleting files

set history=1000                           " remember more commands and search history
set undolevels=1000                        " use many muchos levels of undo

set ssop-=options                          " dont save global or local values in a session
set ssop-=folds                            " dont save folds in a session

filetype plugin indent on                  " detect filetype and load plugins and indents

set mouse=a                                " enable mouse support

set foldmethod=indent                      " enable folding using indent mode
set foldlevelstart=99                           " make shure no fold is closed when opening

set textwidth=80                           " set default text width to 80
set formatoptions+=t                       " automatically wrap code if text width is reached
set formatoptions+=c                       " automatically wrap comments if text width is reached

" basic vim looks {{{1
" ==============================================================================
syntax on                                  " syntax highlighting on

set title                                  " change the terminal's title

"set nowrap                                 " don't wrap lines
set wrap                                   " wrap lines ...
set linebreak                              " ... at words.
set breakindent                            " Indent broken lines.
set showbreak=\ \                          " Prepend two whitespaces.

set tabstop=2                              " a tab is two spaces
set cursorline                             " mark the current cursor line
set relativenumber                         " always show line numbers
set showmatch                              " show matching parenthesis

set scrolljump=5                           " lines to jump when cursor leaves page
set scrolloff=3                            " always keep this many lines under cursor

set showmode                               " always show the mode
set ls=2                                   " always show name of file

set hlsearch                               " highlight search terms
set incsearch                              " show search matches as you type

set visualbell                             " blink when there is an error
set noerrorbells                           " don't beep

set wildmode=list:longest,full             " look of the wildmenu displayed for completing a command

set colorcolumn=81                         " mark right border

set fillchars="fold: "                     " dont print filling characters on a folded line

set nolist                                 " list is toggled with a hotkey
set listchars=tab:>-,eol:¬                 " special characters for list

let base16colorspace=256 
colorscheme base16-atelierforest           " use atelierforest
set background=dark                        " use the dark flavour
function FoldColorSchemeChanges()              " custom changes to the colorscheme regarding folding
  function! MyFoldText()
    let nl = v:foldend - v:foldstart + 1
    let txt = '╰'
    let c = 0
    while c < strlen(v:folddashes)
      let txt .= '┄┄'
      let c += 1
    endwhile
    if &foldmethod != "indent"
      let line = getline(v:foldstart)
      let sub = substitute(line, '/\*\|\*/\|{{{\d\=', '', 'g')
      let txt .= ' ' . sub . '(' . nl . ')'
    else
      let txt .= nl
    endif
    return txt
  endfunction
  set foldtext=MyFoldText()
  highlight Folded term=NONE cterm=NONE gui=NONE ctermbg=None  " dont underline a folded line
  if &background == "dark"
    highlight Folded ctermbg=0 ctermfg=10 guifg=blue
  else
    highlight Folded ctermbg=7 ctermfg=12 guifg=blue
  endif
endfunction
call FoldColorSchemeChanges()                  " apply the colorscheme changes
highlight YcmErrorSection ctermbg=NONE ctermfg=NONE cterm=italic
highlight YcmErrorLine ctermbg=18 ctermfg=None
highlight YcmErrorSign ctermbg=18 ctermfg=1




" vim plugins {{{1
" ==============================================================================
let g:NERDTreeWinSize = 25                 " NERDTree size
" close vim if the only open window is NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && 
      \ b:NERDTreeType == "primary") | q | endif


let delimitMate_expand_cr = 1

let NERDTreeQuitOnOpen=1

let g:snips_author="Markus Frey"
let g:snips_location="Freiburg"
let g:UltiSnipsSnippetDirectories=["UltiSnippets"]  " Where to look for snippets
let g:UltiSnipsEnableSnipMate=1
" let g:ycm_enable_diagnostic_signs=0
" let g:ycm_enable_diagnostic_highlighting=0
let g:fuf_coveragefile_globPatterns = ['**/*.h', '**/*.cpp', '**.*.c', '**/*.xml', '**/*.qml', '**/*.txt', '**/*.py']
let g:fuf_coveragefile_exclude = '\v\~$|\.o$|\.exe$|\.class$|^build|^external'

let g:ycm_autoclose_preview_window_after_insertion = 1
" make YCM compatible with UltiSnips (using supertab)
set completeopt=menu
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:ycm_add_preview_to_completeopt = 0
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:ycm_always_populate_location_list = 1

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<cr>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

let g:lt_location_list_toggle_map = '<F11>'
let g:lt_quickfix_list_toggle_map = '<F12>'

" functions {{{1
" ==============================================================================
" Toggle between .h and .cpp files. Create them if not existent.
function! ToggleBetweenHeaderAndSourceFile()
  let bufname = bufname("%")
  let ext = fnamemodify(bufname, ":e")
  let seperateIncDir = 0

  let splitName = split(bufname, '/')
  let idx = len(splitName) - 1
  while idx > -1
    if splitName[idx] == "include"
      let splitName[idx] = "src"
      break
    elseif splitName[idx] == "src"
      " Header file in same directory?
      if (filereadable(fnamemodify(bufname, ":r") . "." . "h") != 0)
        break
      else
        let splitName[idx] = "include"
        break
      endif
    endif
    let idx = idx - 1
  endwhile

  let bufname = join(splitName, '/')
  if ext == "h"
    if (filereadable(fnamemodify(bufname, ":r") . "." . "cu"))
      let ext = "cu"
    else
      let ext = "cpp"
    endif
  elseif ext == "cpp" || ext == "cu"
    let ext = "h"
  else
    return
  endif

  let bufname_new = fnamemodify(bufname, ":r") . "." . ext
  let bufname_alt = bufname("#")
  if bufname_new == bufname_alt
    execute ":e#"
  else
    execute ":e " . fnameescape(bufname_new)
  endif
endfunction


" hotkeys {{{1
" ==============================================================================
nnoremap ; :
nnoremap <S-l> $
nnoremap <S-h> ^
oma <S-l> $
oma <S-h> ^
nnoremap < <<
nnoremap > >>

" paste insert mode
set pastetoggle=<F2>

" mouse wheel up scrolling
map <ScrollWheelUp> 6<C-Y>
" mouse wheel down scrolling
map <ScrollWheelDown> 6<C-E>

" move window focus to the left
map <C-h> <C-w>h
" move window focus to the right
map <C-l> <C-w>l
" move window focus down
map <C-j> <C-w>j
" move window focus up
map <C-k> <C-w>k

" new tab with Alt-n
nnoremap n :tabnew<CR>
" close tab with Alt-q
nnoremap q :tabclose<CR>
" focus previous tab (normal mode)
nnoremap <F7> :tabprevious<CR>
" focus next tab (normal mode)
nnoremap <F8> :tabnext<CR>
" focus previuos tab (insert mode)
inoremap <F7> <Esc>:tabprevious<CR>
" focus next tab (insert mode)
inoremap <F8> <Esc>:tabnext<CR>
" move current tab to the left
nnoremap <silent> <C-F7> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
" move current tab to the right
nnoremap <silent> <C-F8> :execute 'silent! tabmove ' . tabpagenr()<CR>

" open NERDTree
nnoremap <F9> :NERDTreeToggle<CR>
inoremap <F9> <Esc>:NERDTreeToggle<CR>

" open Tagbar
nnoremap <F10> :TagbarToggle<CR><C-w>l
inoremap <F10> <Esc>:TagbarToggle<CR><C-w>l

" clear word highlights from search
nnoremap <silent> <leader>, :nohlsearch<CR>
" use F4 to toggle between .h and .cpp files
map <silent> <F4> :call ToggleBetweenHeaderAndSourceFile()<CR>

" toggle light/dark color scheme (if supported by the colorscheme)
call togglebg#map("")
nnoremap <F5> :ToggleBG<CR>:call FoldColorSchemeChanges()<CR>
inoremap <F5> :ToggleBG<CR>:call FoldColorSchemeChanges()<CR>
vnoremap <F5> :ToggleBG<CR>:call FoldColorSchemeChanges()

" " move over closing delimiter with Ctrl-Space
" inoremap <C-@> <C-R>=delimitMate#JumpAny("\<C-@>")<CR>
inoremap <C-Space> <C-R>=delimitMate#JumpAny("\<C-Space>")<CR>

" move everything from the cursor to the end of the line atop of the current line
nnoremap <S-k> :execute "normal! d$:" . (line(".") - 1) ."put\<lt>CR>"<CR>

nnoremap 'f :FufCoverageFile<cr>
nnoremap 'h :FufFile $HOME/<cr>
nnoremap 'k :FufBuffer<cr>
nnoremap 'd :FufDir<cr>

"let g:ycm_key_list_select_completion=[]
"let g:ycm_key_list_previous_completion=[]

" UltiSnips
" let g:UltiSnipsExpandTrigger="<NUL>"
" let g:UltiSnipsJumpForwardTrigger=""
" let g:UltiSnipsJumpBackwardTrigger=""
" let g:UltiSnipsJumpForwardTrigger="<c-n>"

" filetype specific {{{1
" ==============================================================================
"
if has('autocmd')
  autocmd! BufNewFile,BufRead *.pde setlocal ft=arduino
  autocmd! BufNewFile,BufRead *.ino setlocal ft=arduino
  autocmd filetype c,cpp,arduino call FtCSettings()
  autocmd filetype vim,sh call ShSettings()
  autocmd filetype tex call TexSettings()
endif

function FtCSettings()
  autocmd BufEnter *.cpp,*.h,*.ino,*.pde highlight OverLength ctermbg=52 guibg=#592929  " color for overlength
  autocmd BufEnter *.cpp,*.h,*.ino,*.pde match OverLength /\%81v.*/  " highlight lines longer than 80 chars
  autocmd BufEnter *.cpp,*.h,*.ino,*.pde set foldmethod=indent
  autocmd BufWritePre *.cpp :%s/\s\+$//e   " delete unneccesary whitespaces on save
  autocmd BufWritePre *.c :%s/\s\+$//e   " delete unneccesary whitespaces on save
  autocmd BufWritePre *.h :%s/\s\+$//e     " delete unneccesary whitespaces on save
  let &path.="include"  " so gf finds files
endfunction

function ShSettings()
  set foldmethod=marker  "use marker folding
  execute "normal! zM"
endfunction

function TexSettings()
  " wrap text but only insert linebreaks when explicitly entered
  :set wrap
  :set linebreak
  :set nolist  " list disables linebreak
  :set textwidth=0
  :set wrapmargin=0
endfunction

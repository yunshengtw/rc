set tabstop=4
set sw=4
syntax on
set hlsearch
set nu
set expandtab
set smartindent
set laststatus=2

"Spell check
autocmd FileType tex,markdown,text set spell |
                             set spelllang=en
command Showspell view myspell

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25

let &titlestring = "vim(" . expand("%:t") . ")"
if &term == "screen-256color"
  set t_ts=k
  set t_fs=\
endif
if &term == "screen-256color" || &term == "xterm"
  set title
  set titleold=
endif

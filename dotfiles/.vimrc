" plugin manager
call plug#begin('~/.vim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
call plug#end()
" general settings
set background=dark
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set timeoutlen=1000
set ttimeoutlen=0
" plugin settings
let g:nord_italic_comments = 1
" let g:airline_theme='nord'
let g:javascript_plugin_jsdoc = 1

colorscheme hybrid

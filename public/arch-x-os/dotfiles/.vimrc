" plugin manager
call plug#begin('~/.vim/plugged')
Plug 'w0rp/ale'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
call plug#end()
" general settings
filetype plugin indent on
set guifont=Operator Mono Lig:h10
set tabstop=2
set shiftwidth=2
set laststatus=2
set ttimeoutlen=50
set expandtab
" nord theme settings
let g:nord_italic_comments = 1
" vim-airline configuration
let g:airline_theme='nord'
" vim-javascript configuration
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
" initialization
colorscheme nord

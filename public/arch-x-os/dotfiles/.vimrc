" plugin manager
call plug#begin('~/.vim/plugged')
Plug 'w0rp/ale'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'arcticicestudio/nord-vim'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
call plug#end()
" general settings
set guifont=Input:h12
set tabstop=2
set softtabstop=0 noexpandtab
set shiftwidth=2
set laststatus=2
set ttimeoutlen=50
" powerline integration
" set rtp+=/usr/lib/python3.6/site-packages/powerline/bindings/vim
" set laststatus=2
" set t_Co=256
" nord theme settings
let g:nord_italic_comments = 1
" vim-airline configuration
let g:airline_theme='nord'
" vim-javascript configuration
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
" initialization
colorscheme nord
" keybinds
map <C-n> :NERDTreeToggle<CR>

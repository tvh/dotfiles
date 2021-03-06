filetype plugin indent on
syntax enable
set mouse=a
set softtabstop=4 shiftwidth=4 expandtab

" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin()
" Autocompletion
function! DoRemote(arg)
  UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote'), 'for': 'haskell' }

" Status bar
Plug 'bling/vim-airline'

" Color Scheme
Plug 'tomasr/molokai'

" Haskell
Plug 'parsonsmatt/intero-neovim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }

" Scala
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }

" Python
Plug 'zchee/deoplete-jedi', { 'for': 'python' }

"Markdown
Plug 'vim-pandoc/vim-pandoc', { 'for': 'markdown' }
Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': 'markdown' }

"Rust
Plug 'rust-lang/rust.vim', { 'for': 'rust' }

"JavaScript
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }

"TypeScript
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }

"HTML/CSS
Plug 'mattn/emmet-vim', { 'for': ['html','css'] }

"ORG
Plug 'tpope/vim-speeddating' | Plug 'jceb/vim-orgmode', { 'for': 'org' }

"Git
Plug 'tpope/vim-git', {'for': ['git', 'gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail']}

" Add plugins to &runtimepath
call plug#end()

"""Colorscheme
colorscheme molokai
let g:rehash256 = 1

""" Whitespace
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
set list
let &colorcolumn=join(range(101,999),",")

""" Use deoplete (Autocompletion)
let g:deoplete#enable_at_startup = 1
let g:deoplete#disable_auto_complete = 1

""" Haskell
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1

let g:haskell_indent_if = 0
let g:haskell_indent_case = 2
let g:haskell_indent_let = 4
let g:haskell_indent_where = 2
let g:haskell_indent_do = 3
let g:haskell_indent_in = 0
let g:haskell_indent_guard = 4
let g:cabal_indent_section = 2

let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

let g:necoghc_enable_detailed_browse = 0

""autocmd BufWritePost *.hs GhcModCheckAsync

"""LaTeX
let g:tex_flavor='latex'

"""HTML/CSS
let g:user_emmet_mode='a'


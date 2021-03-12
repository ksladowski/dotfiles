" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  "autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload/plugged')

    " Better Syntax Support
    Plug 'sheerun/vim-polyglot'
    " Auto pairs for '(' '[' '{'
    Plug 'jiangmiao/auto-pairs'
    Plug 'tpope/vim-surround'
    " Lightline status line
    Plug 'itchyny/lightline.vim'
    " Nord Theme
    Plug 'arcticicestudio/nord-vim'
    " Stable version of coc
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    "vifm
    Plug 'vifm/vifm.vim'
    "Commenting plugin
    Plug 'tpope/vim-commentary'
    "highlighter for hex color codes
    Plug 'norcalli/nvim-colorizer.lua'
    "colorize matching parentheticals
    Plug 'junegunn/rainbow_parentheses.vim'
    "search functionality
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'airblade/vim-rooter'
    "start screen
    Plug 'mhinz/vim-startify' 
    "navigation plugins
    Plug 'unblevable/quick-scope'
    Plug 'justinmk/vim-sneak'
    "which key
    Plug 'liuchengxu/vim-which-key'
    "git integration
    Plug 'mhinz/vim-signify'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-rhubarb'
    Plug 'junegunn/gv.vim'

    "cant sudowrite using nvim, author marked issue as 'wontfix'
    Plug 'lambdalisue/suda.vim'

    "document editing
    Plug 'lervag/vimtex'

    call plug#end()

"General
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/general/mappings.vim
source $HOME/.config/nvim/general/termdebug.vim

"Load Plugins
source $HOME/.config/nvim/vim-plug/plugins.vim

"Plugin Settings
source $HOME/.config/nvim/plug-conf/lightline.vim
source $HOME/.config/nvim/plug-conf/nord.vim
source $HOME/.config/nvim/plug-conf/coc.vim
source $HOME/.config/nvim/plug-conf/vifm.vim
source $HOME/.config/nvim/plug-conf/commentary.vim
source $HOME/.config/nvim/plug-conf/rainbow.vim
source $HOME/.config/nvim/plug-conf/search.vim
source $HOME/.config/nvim/plug-conf/startscreen.vim
source $HOME/.config/nvim/plug-conf/quickscope.vim
source $HOME/.config/nvim/plug-conf/sneak.vim
source $HOME/.config/nvim/plug-conf/whichkey.vim
source $HOME/.config/nvim/plug-conf/suda.vim
source $HOME/.config/nvim/plug-conf/latex.vim
lua require'plug-colorizer'

"Re-enable transparency. Nord disables it and it only seems to work when reset
"here
hi! Normal ctermbg=NONE guibg=NONE


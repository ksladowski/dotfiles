packadd termdebug

let g:termdebug_wide=1

nnoremap <Leader>dc :!gcc -g -Wall -Werror % -o 
nnoremap <Leader>dt :Termdebug
nnoremap <Leader>djg :Gdb<CR>
nnoremap <Leader>djp :Program<CR>
nnoremap <Leader>djs :Source<CR>
nnoremap <Leader>db :Break<CR>
nnoremap <Leader>ds :Step<CR>
nnoremap <Leader>de :Evaluate<CR>

vim.keymap.set("n", "<leader>`", "<cmd>terminal<cr>", { desc = "Launch terminal in current window" })

-- Exit with EscEsc instead of obscure C-\ C-n chord
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

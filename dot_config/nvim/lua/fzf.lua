vim.pack.add({ "https://github.com/ibhagwan/fzf-lua" })

require("fzf-lua").setup({
    { "default-prompt" },
})

vim.keymap.set("n", "<leader> ", "<cmd>FzfLua global<cr>", { desc = "Global Fzf" })
vim.keymap.set("n", "<leader>:", "<cmd>FzfLua command_history<cr>", { desc = "Command History" })
vim.keymap.set("n", "<leader>/", "<cmd>FzfLua grep_curbuf<cr>", { desc = "Grep current buffer" })
vim.keymap.set("n", "<leader>s", "<cmd>FzfLua live_grep_native<cr>", { desc = "Search cwd with rg" })
vim.keymap.set("n", "<leader>hm", "<cmd>FzfLua manpages<cr>", { desc = "Manpages" })
vim.keymap.set("n", "<leader>hn", "<cmd>FzfLua helptags<cr>", { desc = "Nvim help" })
vim.keymap.set("n", "<leader>hk", "<cmd>FzfLua keymaps<cr>", { desc = "Keybindings" })
vim.keymap.set("n", "<leader>ff", "<cmd>FzfLua files<cr>", { desc = "Files" })
vim.keymap.set("n", "<leader>fb", "<cmd>FzfLua buffers<cr>", { desc = "Buffers" })

vim.keymap.set({ "n", "x" }, "gra", "<cmd>FzfLua lsp_code_actions<cr>", { desc = "Code Actions" })

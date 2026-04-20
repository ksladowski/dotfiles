vim.opt.splitright = true
vim.opt.splitbelow = true

vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })
vim.keymap.set("n", "<C-S-h>", "<C-w>H", { desc = "Move window to the left" })
vim.keymap.set("n", "<C-S-l>", "<C-w>L", { desc = "Move window to the right" })
vim.keymap.set("n", "<C-S-j>", "<C-w>J", { desc = "Move window to the lower" })
vim.keymap.set("n", "<C-S-k>", "<C-w>K", { desc = "Move window to the upper" })

vim.pack.add({ "https://github.com/s1n7ax/nvim-window-picker" })
require("window-picker").setup({
    hint = "floating-letter",
})

-- TODO Currently not doing anything with this, it returns a window id and operating on that id is beyond the scope
vim.keymap.set("n", "<leader>w", function()
    require("window-picker").pick_window()
end, { desc = "Window Picker" })

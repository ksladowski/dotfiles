vim.pack.add({ "https://github.com/OXY2DEV/markview.nvim" })

vim.pack.add({ "https://github.com/obsidian-nvim/obsidian.nvim" })

if vim.fn.isdirectory("~/Documents/notes") == 0 then
    return
end

require("obsidian").setup({
    legacy_commands = false, -- hides warning on startup, will go away in 4.0
    workspaces = {
        {
            name = "notes",
            path = "~/Documents/notes",
        },
    },
})

vim.keymap.set("n", "<leader>O", "<cmd>Obsidian<cr>", { desc = "Obsidian" })

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- system clipboard integration
vim.opt.clipboard = "unnamedplus"

-- Indentation
vim.opt.tabstop = 4
vim.opt.shiftwidth = 0 -- 0 uses tabstop value
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smartindent = true -- changes autoindent behavior to auto indent functions

vim.opt.swapfile = false
vim.opt.autoread = true -- Monitor open files for changes
vim.opt.undofile = true

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>") -- Clear highlights on search when pressing <Esc> in normal mode

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- Decrease update time
vim.o.updatetime = 250

-- Decrease mapped sequence wait time
vim.o.timeoutlen = 300

vim.opt.confirm = true

-- Plugin dependencies
vim.pack.add({ "https://github.com/nvim-lua/plenary.nvim" })
vim.pack.add({ "https://github.com/nvim-tree/nvim-web-devicons" })
vim.pack.add({ "https://github.com/MunifTanjim/nui.nvim" })
vim.pack.add({ "https://github.com/3rd/image.nvim" })

require("ui")
require("windows")
require("fzf")
require("git")
require("filemanager")
require("editing")
require("completion")
require("formatting")
require("lsp")
require("notes")
require("debugging")
require("terminal")
require("ai")

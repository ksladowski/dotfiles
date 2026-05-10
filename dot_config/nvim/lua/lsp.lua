-- Yeah I know its not specifically LSP stuff but its all the same category imo

vim.pack.add({ "https://github.com/neovim/nvim-lspconfig" })                   -- nice preconfigured lsp settings. Don't need to do anything to set them up other than vim.lsp.enable
vim.pack.add({ "https://github.com/nvim-treesitter/nvim-treesitter-context" }) -- if a method is too long for one screen, still see the header

-- Diagnostic Config & Keymaps
vim.diagnostic.config({
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = " ",
            [vim.diagnostic.severity.WARN] = " ",
            [vim.diagnostic.severity.INFO] = " ",
            [vim.diagnostic.severity.HINT] = " ",
        },
    },

    -- Can switch between these as you prefer
    virtual_text = true, -- Text shows up at the end of the line
    -- virtual_lines = false, -- Text shows up underneath the line, with virtual lines

    -- Auto open the float, so you can easily read the errors when jumping with `[d` and `]d`
    jump = { float = true },

    update_in_insert = false,
    severity_sort = true,
    float = { border = "rounded", source = "if_many" },
    underline = { severity = { min = vim.diagnostic.severity.WARN } },
})

vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

-- Non NixOS Only
if vim.fn.isdirectory("/etc/NIXOS") == 0 then
    vim.pack.add({ "https://github.com/mason-org/mason.nvim" })
    require("mason").setup({
        registries = {
            "github:mason-org/mason-registry",
            "github:Crashdummyy/mason-registry",
        }
    })
    vim.pack.add({ "https://github.com/nvim-treesitter/nvim-treesitter" })
    require('nvim-treesitter').install { 'lua', 'c_sharp', 'razor' }
end

require("lsp.lua")
require("lsp.nix")
require("lsp.dotnet")

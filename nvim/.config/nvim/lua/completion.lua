-- Blink
vim.pack.add({
    {
        src = "https://github.com/saghen/blink.cmp",
        version = vim.version.range("^1"), -- pin to stable, v2 will require external binaries
    },
})

require("blink.cmp").setup({
    completion = {
        documentation = { auto_show = true },
    },
    sources = {
        default = { "lsp", "path", "snippets", "buffer" },
    },
    fuzzy = {
        implementation = "prefer_rust_with_warning",
    },
})

-- TODO LuaSnip

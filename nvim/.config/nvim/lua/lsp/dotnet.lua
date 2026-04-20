vim.pack.add({ "https://github.com/seblyng/roslyn.nvim" })

vim.lsp.config("roslyn", {
    settings = {
        ["csharp|inlay_hints"] = {
            csharp_enable_inlay_hints_for_implicit_object_creation = true,
            csharp_enable_inlay_hints_for_implicit_variable_types = true,
        },
        ["csharp|code_lens"] = {
            dotnet_enable_references_code_lens = true,
        },
    },
})

vim.pack.add({ "https://github.com/GustavEikaas/easy-dotnet.nvim" })
require("easy-dotnet").setup({
    lsp = {
        enabled = false -- need razor support, until it gets added, will be using roslyn.nvim
    }
})

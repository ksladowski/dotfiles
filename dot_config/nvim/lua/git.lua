vim.pack.add({ "https://github.com/lewis6991/gitsigns.nvim" })
require("gitsigns").setup({
    on_attach = function(bufnr)
        local gitsigns = require("gitsigns")

        local function map(mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map("n", "]c", function()
            if vim.wo.diff then
                vim.cmd.normal({ "]c", bang = true })
            else
                gitsigns.nav_hunk("next")
            end
        end, { desc = "Jump to next git [c]hange" })

        map("n", "[c", function()
            if vim.wo.diff then
                vim.cmd.normal({ "[c", bang = true })
            else
                gitsigns.nav_hunk("prev")
            end
        end, { desc = "Jump to previous git [c]hange" })

        -- Actions
        -- visual mode
        map("v", "<leader>ghs", function()
            gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { desc = "[g]it [h]unk [s]tage" })
        map("v", "<leader>ghr", function()
            gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
        end, { desc = "[g]it [h]unk [r]eset" })
        -- normal mode
        map("n", "<leader>ghs", gitsigns.stage_hunk, { desc = "git [h]unk [s]tage" })
        map("n", "<leader>ghr", gitsigns.reset_hunk, { desc = "git [h]unk [r]eset" })
        map("n", "<leader>gbS", gitsigns.stage_buffer, { desc = "git [S]tage buffer" })
        map("n", "<leader>ghu", gitsigns.stage_hunk, { desc = "git [u]ndo stage hunk" })
        map("n", "<leader>ghR", gitsigns.reset_buffer, { desc = "git [R]eset buffer" })
        map("n", "<leader>ghp", gitsigns.preview_hunk, { desc = "git [p]review hunk" })
        map("n", "<leader>ghb", gitsigns.blame_line, { desc = "git [b]lame line" })
        map("n", "<leader>ghd", gitsigns.diffthis, { desc = "git [d]iff against index" })
        map("n", "<leader>ghD", function()
            gitsigns.diffthis("@")
        end, { desc = "git [D]iff against last commit" })
        -- Toggles
        map("n", "<leader>tgb", gitsigns.toggle_current_line_blame, { desc = "[T]oggle git show [b]lame line" })
        map("n", "<leader>tgD", gitsigns.preview_hunk_inline, { desc = "[T]oggle git show [D]eleted" })
    end,
})

vim.pack.add({ "https://github.com/sindrets/diffview.nvim" })
require("diffview").setup()

vim.pack.add({ "https://github.com/NeogitOrg/neogit" })
vim.keymap.set("n", "<leader>gg", function()
    require("neogit").open()
end, { desc = "Open Neogit UI" })

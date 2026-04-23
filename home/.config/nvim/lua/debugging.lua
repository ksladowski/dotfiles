vim.pack.add({ "https://github.com/mfussenegger/nvim-dap" })
vim.pack.add({ "https://github.com/rcarriga/nvim-dap-ui" })
vim.pack.add({ "https://github.com/nvim-neotest/nvim-nio" })
vim.pack.add({ "https://github.com/theHamsta/nvim-dap-virtual-text" })

local dap = require("dap")
require("nvim-dap-virtual-text").setup({})

local ui = require("dapui")
ui.setup()

vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "Toggle breakpoint" })
vim.keymap.set("n", "<leader>dC", dap.run_to_cursor, { desc = "Run to Cursor" })

vim.keymap.set("n", "<leader>dc", dap.continue)

vim.keymap.set("n", "<leader>di", dap.step_into)
vim.keymap.set("n", "<leader>ds", dap.step_over)
vim.keymap.set("n", "<leader>do", dap.step_out)
vim.keymap.set("n", "<leader>db", dap.step_back)
vim.keymap.set("n", "<leader>dR", dap.restart)

dap.listeners.before.attach.dapui_config = function()
    ui.open()
end
dap.listeners.before.launch.dapui_config = function()
    ui.open()
end
dap.listeners.before.event_terminated.dapui_config = function()
    ui.close()
end
dap.listeners.before.event_exited.dapui_config = function()
    ui.close()
end

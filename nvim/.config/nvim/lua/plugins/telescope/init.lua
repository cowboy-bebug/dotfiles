local map = vim.api.nvim_set_keymap
local opts = {
    noremap = true,
    silent = true,
}

map("n", "<leader>ff", ":Telescope find_files<CR>", opts)
map("n", "<leader>fg", ":Telescope live_grep<CR>", opts)

require("telescope").setup({
    defaults = {
        file_ignore_patterns = { ".git" },
    },
    pickers = {
        find_files = { hidden = true },
    },
})

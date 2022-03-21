local map = vim.api.nvim_set_keymap
local opts = {
    noremap = true,
    silent = true,
}

map("n", "<C-h>", ":BufferPrevious<CR>", opts)
map("n", "<C-l>", ":BufferNext<CR>", opts)
map("n", "<C-p>", ":BufferPick<CR>", opts)
map("n", "<C-c>", ":BufferClose<CR>", opts)

vim.g.bufferline = {
    animation = true,
    closable = true,
    clickable = true,
    icon_separator_active = "",
    icon_separator_inactive = "",
}

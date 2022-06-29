local opts = {
  noremap = true,
  silent = true,
}

vim.api.nvim_set_keymap("n", "<leader>e", ":NvimTreeToggle<CR>", opts)

require("nvim-tree").setup({
  diagnostics = { enable = true },
  filters = {
    dotfiles = false,
    custom = { "^.git$" },
  },
  git = { ignore = false },
})

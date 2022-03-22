local opts = {
    noremap = true,
    silent = true,
}

vim.api.nvim_set_keymap("n", "<leader>e", ":NvimTreeToggle<CR>", opts)

vim.g.nvim_tree_icons = {
    git = {
        unstaged = "✗",
        staged = "✓",
        unmerged = "",
        renamed = "➜",
        untracked = "★",
        deleted = "",
        ignored = "◌",
    },
}

require("nvim-tree").setup({
    auto_close = false,
    diagnostics = { enable = true },
    filters = {
        dotfiles = false,
        custom = { ".git", "target" },
    },
    actions = {
        open_file = {
            quit_on_open = false,
        },
    },
})

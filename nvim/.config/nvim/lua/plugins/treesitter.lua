require("nvim-treesitter.configs").setup({
    ensure_installed = {
        "bash",
        "dockerfile",
        "go",
        "gomod",
        "hcl",
        "http",
        "javascript",
        "json",
        "json5",
        "jsonc",
        "lua",
        "markdown",
        "python",
        "regex",
        "ruby",
        "rust",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "yaml",
    },

    sync_install = false,

    highlight = {
        enable = true,
        additional_vim_regex_highlighting = true,
    },

    refactor = {
        highlight_definitions = {
            enable = true,
            clear_on_cursor_move = true,
        },
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = "grr",
            },
        },
        navigation = {
            enable = true,
            keymaps = {
                goto_definition = "gnd",
                list_definitions = "gnD",
                list_definitions_toc = "gO",
                goto_next_usage = "<a-*>",
                goto_previous_usage = "<a-#>",
            },
        },
    },
})

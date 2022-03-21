local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.formatting.black,
        null_ls.builtins.formatting.lua_format.with({
            extra_args = {
                "--single-quote-to-double-quote",
                "--column-limit=100",
                "--column-table-limit=30",
                "--chop-down-table",
                "--extra-sep-at-table-end",
                "--spaces-inside-table-braces",
            },
        }),
        null_ls.builtins.formatting.prettier.with({
            filetypes = {
                "javascript",
                "javascriptreact",
                "typescript",
                "typescriptreact",
                "vue",
                "css",
                "scss",
                "less",
                "html",
                "json",
                "jsonc",
                "yaml",
                "markdown",
                "graphql",
                "handlebars",
            },
        }),
        null_ls.builtins.formatting.rustfmt,
        null_ls.builtins.formatting.taplo,
        null_ls.builtins.formatting.terraform_fmt,
    },
    on_attach = function(client)
        if client.resolved_capabilities.document_formatting then
            vim.cmd([[
            augroup LspFormatting
                autocmd! * <buffer>
                autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()
            augroup END
            ]])
        end
    end,
})

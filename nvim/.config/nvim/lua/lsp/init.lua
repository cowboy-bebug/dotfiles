-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local map = vim.api.nvim_set_keymap
local opts = {
    noremap = true,
    silent = true,
}

map("n", "<space>of", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
map("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
map("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
map("n", "<space>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local buf_map = vim.api.nvim_buf_set_keymap
    buf_map(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
    buf_map(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_map(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
    buf_map(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_map(bufnr, "n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
    buf_map(bufnr, "n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_map(bufnr, "n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    buf_map(bufnr, "n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    buf_map(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_map(bufnr, "n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

    -- hack: disable diagnostic for helm files
    if vim.bo[bufnr].buftype ~= "" or vim.bo[bufnr].filetype == "helm" then
        vim.diagnostic.disable(bufnr)
        vim.defer_fn(function() vim.diagnostic.reset(nil, bufnr) end, 1000)
    end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

for _, lsp in pairs({
    "bashls",
    "dockerls",
    "gopls",
    "pyright",
    "rust_analyzer",
    "terraformls",
}) do
    require("lspconfig")[lsp].setup({
        on_attach = on_attach,
        capabilities = capabilities,
        flags = {
            debounce_text_changes = 150,
        },
    })
end

require("lsp/cmp")
require("lsp/installer")

require("nvchad.configs.lspconfig").defaults()

local configs = require "nvchad.configs.lspconfig"

local servers = { "html", "cssls", "ts_ls" }

for _, lsp in ipairs(servers) do
  vim.lsp.config(lsp, {
    on_attach = configs.on_attach,
    on_init = configs.on_init,
    capabilities = configs.capabilities,
  })
  vim.lsp.enable(lsp)
end

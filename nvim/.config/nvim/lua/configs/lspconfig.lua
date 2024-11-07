require("nvchad.configs.lspconfig").defaults()

local lspconfig = require "lspconfig"

local servers = { "html", "cssls", "ts_ls" }
local configs = require "nvchad.configs.lspconfig"

for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = configs.on_attach,
    on_init = configs.on_init,
    capabilities = configs.capabilities,
  }
end

local autocmd = vim.api.nvim_create_autocmd

-- Auto resize panes when resizing nvim window
autocmd("VimResized", {
	pattern = "*",
	command = "tabdo wincmd =",
})

vim.opt.relativenumber = true

vim.opt.spell = true
vim.opt.spellcapcheck = ""
vim.opt.spelllang = "en_nz,en_us"
vim.opt.spelloptions = "camel"

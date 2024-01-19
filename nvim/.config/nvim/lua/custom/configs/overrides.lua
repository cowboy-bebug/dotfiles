local M = {}

-- For the list of parsers: https://github.com/nvim-treesitter/nvim-treesitter#supported-languages
M.treesitter = {
	ensure_installed = {
		-- defaults
		"vim",
		"lua",

		-- web dev
		"html",
		"css",
		"javascript",
		"typescript",
		"tsx",
		"json",

		-- docs
		"markdown",
		"markdown_inline",
		"org",
	},
	indent = {
		enable = true,
		-- disable = {
		--   "python"
		-- },
	},
}

M.mason = {
	ensure_installed = {
		-- lua stuff
		"lua-language-server",
		"stylua",

		-- web dev stuff
		"css-lsp",
		"html-lsp",
		"typescript-language-server",
		"deno",
		"prettier",
	},
}

M.nvimtree = {
	git = {
		enable = true,
	},
	renderer = {
		highlight_git = true,
		icons = {
			show = {
				git = true,
			},
		},
	},
	view = {
		width = 40,
	},
}

return M

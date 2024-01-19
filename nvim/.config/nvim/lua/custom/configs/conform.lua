local options = {
	lsp_fallback = true,

	formatters_by_ft = {
		["*"] = {
			"codespell",
			"trim_newlines",
			"trim_whitespace",
		},

		lua = { "stylua" },
		sh = { "shfmt" },

		typescript = { "prettier" },
		javascript = { "prettier" },
		css = { "prettier" },
		html = { "prettier" },

		markdown = { "prettier" },
	},

	format_on_save = {
		timeout_ms = 500,
		lsp_fallback = false,
	},

	formatters = {
		prettier = {
			prepend_args = { "--prose-wrap=always" },
		},
	},
}

require("conform").setup(options)

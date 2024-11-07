local options = {
  formatters_by_ft = {
    ["*"] = {
      "codespell",
      "trim_newlines",
      "trim_whitespace",
    },
    css = { "prettier" },
    html = { "prettier" },
    javascript = { "prettier" },
    lua = { "stylua" },
    markdown = { "prettier" },
    sh = { "shfmt" },
    typescript = { "prettier" },
  },

  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },

  formatters = {
    prettier = {
      prepend_args = { "--prose-wrap=always" },
    },
  }
}

return options

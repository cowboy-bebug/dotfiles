function _G.trim_trailing_whitespace()
    local pos = vim.api.nvim_win_get_cursor(0)
    vim.api.nvim_command("silent keepjumps keeppatterns %s/\\s\\+$//e")
    vim.api.nvim_win_set_cursor(0, pos)
end

vim.api.nvim_exec([[
    autocmd BufWritePre * lua trim_trailing_whitespace()
]], false)

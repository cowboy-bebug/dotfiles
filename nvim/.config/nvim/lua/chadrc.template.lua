-- This file needs to have same structure as nvconfig.lua
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :(

---@type ChadrcConfig
local M = {}

M.base46 = {
	theme = "${NVIM_THEME}",

	-- hl_override = {
	-- 	Comment = { italic = true },
	-- 	["@comment"] = { italic = true },
	-- },
}

vim.api.nvim_create_autocmd("User", {
	pattern = "VeryLazy", -- ensures it runs after NvChad lazy loading
	callback = function()
		require("base46").load_all_highlights()
	end,
})

return M

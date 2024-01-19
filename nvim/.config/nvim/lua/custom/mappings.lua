---@type MappingsTable
local M = {}

-- Refer to: https://github.com/NvChad/NvChad/blob/v2.0/lua/core/mappings.lua
M.general = {
	n = {
		[";"] = { ":", "enter command mode", opts = { nowait = true } },

		["<leader>fm"] = {
			function()
				require("conform").format()
			end,
			"Format with conform",
		},

		["<C-a>"] = { "gg0VG", "Select all", opts = { noremap = true } },
	},
}

return M

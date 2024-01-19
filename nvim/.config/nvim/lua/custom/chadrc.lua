---@type ChadrcConfig
local M = {}

-- Path to overriding theme and highlights files
local highlights = require("custom.highlights")

-- Refer to: https://github.com/NvChad/NvChad/blob/v2.0/lua/core/default_config.lua
M.ui = {
	------------------------------- base46 -------------------------------------
	hl_add = highlights.add,
	hl_override = highlights.override,
	theme_toggle = { "onedark", "onenord" },
	theme = "onenord",
	transparency = false,

	------------------------------- nvchad_ui modules -----------------------------
	statusline = { theme = "vscode" },
	nvdash = { load_on_startup = true },
}

M.plugins = "custom.plugins"

-- check core.mappings for table structure
M.mappings = require("custom.mappings")

return M

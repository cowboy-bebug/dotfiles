local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  packer_bootstrap = vim.fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end

require("packer").startup(function(use)
  use "wbthomason/packer.nvim"
  use "navarasu/onedark.nvim"

  use "kyazdani42/nvim-web-devicons"
  use "kyazdani42/nvim-tree.lua"
  use "nvim-lualine/lualine.nvim"
  use "romgrk/barbar.nvim"

  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  }
  use "nvim-treesitter/nvim-treesitter-refactor"

  use "nvim-lua/plenary.nvim"
  use "nvim-telescope/telescope.nvim"
  use {
    "nvim-telescope/telescope-fzf-native.nvim",
    run = "make",
  }
  use "lewis6991/gitsigns.nvim"

  use "hrsh7th/nvim-cmp" -- Autocompletion plugin
  use "hrsh7th/cmp-nvim-lsp" -- LSP source for nvim-cmp
  use "saadparwaiz1/cmp_luasnip" -- Snippets source for nvim-cmp
  use "L3MON4D3/LuaSnip" -- Snippets plugin
  use "onsails/lspkind-nvim"
  use "jose-elias-alvarez/null-ls.nvim"
  use {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
  }

  use "numToStr/Comment.nvim"
  use "towolf/vim-helm"
  use "norcalli/nvim-colorizer.lua"

  if packer_bootstrap then require("packer").sync() end
end)

require("plugins/barbar")
require("plugins/comment")
require("plugins/colorizer")
require("plugins/gitsigns")
require("plugins/lualine")
require("plugins/null-ls")
require("plugins/telescope")
require("plugins/tree")
require("plugins/treesitter")

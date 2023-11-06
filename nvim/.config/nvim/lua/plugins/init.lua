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

  use "nvim-lualine/lualine.nvim"

  use {
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
  }
  use "nvim-treesitter/nvim-treesitter-refactor"

  -- tree
  use {
    "nvim-tree/nvim-tree.lua",
    requires = { "nvim-tree/nvim-web-devicons" }
  }

  -- telescope
  use {
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
      "nvim-tree/nvim-web-devicons",
    }
  }

  -- cmp
  use {
    "hrsh7th/nvim-cmp",
    requires = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "rafamadriz/friendly-snippets",
    }
  }

  use "hrsh7th/cmp-nvim-lsp" -- LSP source for nvim-cmp
  use "onsails/lspkind-nvim"
  use {
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
  }

  use "lewis6991/gitsigns.nvim"
  use "numToStr/Comment.nvim"

  if packer_bootstrap then require("packer").sync() end
end)

require("Comment").setup()
require("lualine").setup()

require("plugins/gitsigns")
require("plugins/telescope")
require("plugins/tree")
require("plugins/treesitter")

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 3
vim.opt.shiftwidth = 3
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.whichwrap:append("<,>,h,l")

vim.keymap.set("n", "<", "<<", {noremap=true, silent=true})
vim.keymap.set("n", ">", ">>", {noremap=true, silent=true})

vim.cmd [[
  highlight Normal guibg=none
  highlight NonText guibg=none
  highlight Normal ctermbg=none
  highlight NonText ctermbg=none
]]

-- format on save for specific file types
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = { "*.py", "*.c", "*.h" },
  callback = function()
    local filetype = vim.bo.filetype
    local filepath = vim.fn.expand("%:p")

    if filetype == "python" and vim.fn.executable("black") == 1 then
      vim.cmd("write")
      vim.cmd("silent! !black --quiet " .. filepath)
      vim.cmd("edit!")
    elseif (filetype == "c" or filetype == "cpp") and vim.fn.executable("clang-format") == 1 then
      vim.cmd("write")
      vim.cmd("silent! !clang-format -i " .. filepath)
      vim.cmd("edit!")
    end
  end,
})


-- Plugin manager setup
vim.opt.rtp:prepend("~/.config/nvim/lazy/lazy.nvim")

require("lazy").setup({
{
  'echasnovski/mini.nvim',
  version = false, -- latest stable
  config = function()
    require('mini.comment').setup()
    require('mini.pairs').setup()
    require('mini.surround').setup()
    require('mini.statusline').setup()
    -- Add more modules as needed
  end
}
})

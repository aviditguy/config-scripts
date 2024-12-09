vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 3
vim.opt.shiftwidth = 3
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.cursorline = true
vim.opt.whichwrap:append("<,>,h,l")

-- render leading whitespace
vim.opt.list = true
vim.opt.listchars = {tab="· ", lead="·"}

vim.keymap.set("n", "<", "<<", {noremap=true, silent=true})
vim.keymap.set("n", ">", ">>", {noremap=true, silent=true})

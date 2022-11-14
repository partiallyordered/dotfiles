
" TODO:
"  - Possible to display a solid bar at the wrap-around width to indicate visually when lines are
"    over that width?
"  - Often after an easymotion, it's desirable to enter insert mode immediately. Perhaps remap F to
"    do this? I.e. noremap F fXi (where X is the key in question)
"  - Using tcomment, take the <leader>tcy and add a number. E.g. <leader>tc4y to copy the current
"    line and the four following lines comment them, then paste.
"  - Set up ligatures for Haskell:
"    - http://www.bytesizedworkbench.com/blog/2017/10/24/the-joy-of-alacritty/<Paste>
"    - https://github.com/enomsg/vim-haskellConcealPlus
"  - Set up better documentation for Haskell
"  - Consider moving to spacemacs with Haskell layer for Haskell

" From: https://wiki.archlinux.org/index.php/vim/.vimrc
" .vimrc
" See: http://vimdoc.sourceforge.net/htmldoc/options.html for details

" For multi-byte character support (CJK support, for example):
"set fileencodings=ucs-bom,utf-8,cp936,big5,euc-jp,euc-kr,gb18030,latin1

" lua << EOF
" -- Much help and code from:
" -- - https://oroques.dev/notes/neovim-init/
" -- - https://github.com/ojroques/dotfiles/blob/master/nvim/init.lua
" -- See also:
" -- - https://old.reddit.com/r/neovim/comments/kae0wt/switching_to_lua_in_neovim_config_neanderthal/
" 
" local cmd, fn, g = vim.cmd, vim.fn, vim.g
" -- global-, buffer-, and window-scoped options
" -- Unfortunately setting an option is not as straightforward in Lua as in Vimscript. In Lua you
" -- need to update the global table then either the buffer-scoped or the window-scoped table to
" -- ensure that an option is correctly set. Otherwise some option like expandtab will only be valid
" -- for the starting buffer of a new Neovim instance.
" -- This should be fixed in future: https://github.com/neovim/neovim/pull/13479
" -- In the mean-time, see vim help for a given option to determine which scope it resides in.
" 
" local scopes = {o = vim.o, b = vim.bo, w = vim.wo}
" 
" local function opt(scope, key, value)
"   scopes[scope][key] = value
"   if scope ~= 'o' then scopes['o'][key] = value end
" end
" 
" opt('b', 'tabstop', 4)      -- Number of spaces that a <Tab> in the file counts for.
" 
" opt('b', 'shiftwidth', 4)   -- Number of spaces to use for each step of (auto)indent.
" 
" opt('b', 'expandtab', true) -- Use the appropriate number of spaces to insert a <Tab>.
"                             -- Spaces are used in indents with the '>' and '<' commands
"                             -- and when 'autoindent' is on. To insert a real tab when
"                             -- 'expandtab' is on, use CTRL-V <Tab>.
" 
" opt('b', 'smarttab', true)  -- When on, a <Tab> in front of a line inserts blanks
"                             -- according to 'shiftwidth'. 'tabstop' is used in other
"                             -- places. A <BS> will delete a 'shiftwidth' worth of space
"                             -- at the start of the line.
" 
" opt('w', 'wrap', false)     -- Don't wrap long lines
" 
" EOF

set nowrap          " Don't wrap long lines

set tabstop=4       " Number of spaces that a <Tab> in the file counts for.

set shiftwidth=4    " Number of spaces to use for each step of (auto)indent.

set expandtab       " Use the appropriate number of spaces to insert a <Tab>.
                    " Spaces are used in indents with the '>' and '<' commands
                    " and when 'autoindent' is on. To insert a real tab when
                    " 'expandtab' is on, use CTRL-V <Tab>.

set smarttab        " When on, a <Tab> in front of a line inserts blanks
                    " according to 'shiftwidth'. 'tabstop' is used in other
                    " places. A <BS> will delete a 'shiftwidth' worth of space
                    " at the start of the line.

set showcmd         " Show (partial) command in status line.

"set relativenumber  " Show relative line numbers, except on the current line,
"set number          " which will show the current line number

set showmatch       " When a bracket is inserted, briefly jump to the matching
                    " one. The jump is only done if the match can be seen on the
                    " screen. The time to show the match can be set with
                    " 'matchtime'.

set hlsearch        " When there is a previous search pattern, highlight all
                    " its matches.

set incsearch       " While typing a search command, show immediately where the
                    " so far typed pattern matches.

set ignorecase      " Ignore case in search patterns.

set smartcase       " Override the 'ignorecase' option if the search pattern
                    " contains upper case characters.

set backspace=2     " Influences the working of <BS>, <Del>, CTRL-W
                    " and CTRL-U in Insert mode. This is a list of items,
                    " separated by commas. Each item allows a way to backspace
                    " over something.

set autoindent      " Copy indent from current line when starting a new line
                    " (typing <CR> in Insert mode or when using the "o" or "O"
                    " command).

" TODO: 79 column comments?
set textwidth=99    " Maximum width of text that is being inserted. A longer
                    " line will be broken after white space to get this width.

set formatoptions=c,q,r,t,j " This is a sequence of letters which describes how
                    " automatic formatting is to be done.
                    "
                    " letter    meaning when present in 'formatoptions'
                    " ------    ---------------------------------------
                    " c         Auto-wrap comments using textwidth, inserting
                    "           the current comment leader automatically.
                    " q         Allow formatting of comments with "gq".
                    " r         Automatically insert the current comment leader
                    "           after hitting <Enter> in Insert mode.
                    " t         Auto-wrap text using textwidth (does not apply
                    "           to comments)
                    " j         Sensibly join lines inside comments

set formatoptions-=o " do not automatically insert comments when pressing o/O
                    " inside comments

set ruler           " Show the line and column number of the cursor position,
                    " separated by a comma.
set mouse=a         " Enable the use of the mouse.
set conceallevel=1  " Enable concealing

" Set timeoutlen low in insert mode, so that `fd` to exit insert mode works appropriately, but very
" high in normal mode, so that the leader key doesn't time out quickly.
set timeoutlen=100000
au InsertEnter * set timeoutlen=200
au InsertLeave * set timeoutlen=100000

set laststatus=2
set tags=./tags;
" Use unnamedplus to set the clipboard to the + (shared between vim sessions)
" register
set clipboard=unnamed

" Incrementally change substituted text when using ':%s//'
set inccommand=split

" Make history persistent:
set history=500
set viminfo=%,'20,/100,:100,<500,f1,n~/.vim/viminfo

" Highlight the column/line containing the cursor
set cursorcolumn
hi CursorColumn ctermbg=darkgrey
set cursorline
hi CursorLine ctermbg=darkgrey cterm=none

filetype off

" Allow use of <C-Q> and <C-S> by stopping the TTY from capturing them
" silent !stty -ixon > /dev/null 2>/dev/null
" This doesn't work..
" au VimLeave * !stty ixon > /dev/null 2>/dev/null

if has('nvim')
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
    if $COLORTERM ==# 'truecolor'
        set termguicolors
    endif
    " let g:solarized_italic=0
    " let g:solarized_termcolors=16
    " let g:solarized_style="dark"
    " let g:solarized_contrast="high"
    " set rtp^=/usr/share/vim/vimfiles/
endif

" Do this after adding the old vimfiles to rtp; else plugins may not work
" correctly
filetype plugin indent on

" TODO: can these settings be moved to ~/.vim/after/plugins/ ?
syntax on
" somehow, at the time of writing, OceanicNext causes some pretty annoying rendering issues
colorscheme OceanicNext
set background=dark " When set to "dark", Vim will try to use colors that look
                    " good on a dark background. When set to "light", Vim will
                    " try to use colors that look good on a light background.
                    " Any other value is illegal.
" Adopt the terminal emulator background colour
highlight Normal ctermbg=NONE guibg=NONE
highlight NonText ctermbg=NONE guibg=NONE
" From: https://github.com/mhartington/oceanic-next/tree/5ef31a34204f84714885ae9036f66a626036c3dc#configuration
highlight LineNr guibg=NONE ctermbg=NONE
highlight SignColumn guibg=NONE ctermbg=NONE
highlight EndOfBuffer guibg=NONE ctermbg=NONE

" Wrap h, l (normal mode) and cursor keys (normal & insert mode)
set whichwrap+=<,>,h,l,[,]

" Style the vertical window split bar
set fillchars+=vert:│
highlight VertSplit ctermbg=NONE ctermfg=NONE

function! s:closeiflast()
    " Close vim if there is only one listed (open) buffer; else close the
    " current buffer
    " From http://stackoverflow.com/questions/17931507/vimscript-number-of-listed-buffers
    let b_num = len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
    if (b_num == 1)
        :q
    else
        :bd
    endif
endfunction

nmap <space> <leader>
" noremap <leader>l $
" noremap <leader>h 0
" noremap <leader>m ^
nnoremap Y y$
nnoremap <leader>qq :q<cr>
nnoremap <leader>qa :qa<cr>
nnoremap <leader>bd :call <SID>closeiflast()<CR>
nnoremap <leader>rs :%s/\<<C-R><C-W>\>\C/
nnoremap <leader>ss /\<\><left><left>
xnoremap <leader>rs :s/\<\><left><left>
nnoremap <leader>h :noh<CR>
nnoremap <leader>cl :.!column -t -o " "<CR>
xnoremap <leader>cl :!column -t -o " "<CR>
xnoremap <leader>w :s/\<<C-R><C-W>\>/
" Save if changes have been made
nnoremap <leader>w :update<CR>
cnoremap fd <C-C>
inoremap fd <C-C>
inoremap <C-U> <C-C>cc
vnoremap fd <C-C>
inoremap <Esc> <Nop>
nnoremap Q @
" Convert decimal to hex
" nnoremap <leader>dth mz:read !echo "0x$(echo 'obase=16;ibase=10;<C-R><C-W>' \| bc)"<CR>"zd$`zciw<C-R>z<C-C>jdd
" Convert hex to demical
" nnoremap <leader>htd mz:read !calc <C-R><C-W> \| tr -d "[:space:]"<CR>"zd$`zciw<C-R>z<C-C>jdd

" nnoremap <C-Q> :q<CR>
" nnoremap <C-W> :call <SID>closeiflast()<CR>

" TODO: see the sections about using syntax space errors and showing long lines here:
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
" Show trailing spaces at the end of a line. Show tabs.
exec "set listchars=trail:\uB7,tab:\uBB\uBB"
set list

" TODO: Highlight column 101
call matchadd('ColorColumn', '\%101v', 100)

" TODO: move to ~/.vim/after/plugins/ ?
" UltiSnips options
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
let g:UltiSnipsSnippetDirs = "~/.config/nvim/ultisnips/"

" See https://github.com/pangloss/vim-javascript for other conceal options
" let g:javascript_conceal_arrow_function = "⇒"

" vim-surround options
" lower-case b to surround with ()
let g:surround_97 = "(\r)"
" upper-case B to surround with {}
let g:surround_66 = "{\r}"
" lower-case s to surround with []
let g:surround_115 = "[\r]"
" upper-case s to surround with <>
let g:surround_83 = "[\r]"
" switch bracket functionality so closing and opening brackets do and do not add space respectively
let g:surround_40 = "(\r)"    " opening
let g:surround_41 = "( \r )"  " closing
let g:surround_91 = "[\r]"    " opening
let g:surround_93 = "[ \r ]"  " closing
let g:surround_123 = "{\r}"   " opening
let g:surround_125 = "{ \r }" " closing
" dont do funky html/xml stuff
let g:surround_60 = "<\r>"    " opening
let g:surround_62 = "< \r >"  " closing


" markdown-preview settings
let g:mkdp_auto_start = 1
function! g:Open_browser(url)
    " TODO: try firefox, document why I didn't use it
    silent exec "!chromium --class=markdownpreview --user-data-dir=$HOME/.config/chromium_markdownpreview --force-dark-mode --app=" . a:url . " &"
endfunction
let g:mkdp_browserfunc = 'g:Open_browser'

" vim-gh-line open command
" TODO: this should be browser-selector (because clip-args is one option there)
let g:gh_open_command = 'fn() { echo "$@" | xclip -i -sel p -f | xclip -i -sel s -f | xclip -i -sel c; notify-send "Copied to clipboard"; }; fn '

" vim-autoformat options
let g:formatdef_custom_sql = '"sqlformat --comma-first true --reindent_aligned -k upper --indent_after_first"'
let g:formatters_sql = ['custom_sql']

" ALE options
" https://www.arthurkoziel.com/setting-up-vim-for-yaml/index.html
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
    \ 'yaml': ['yamllint']
    \ }

" Enable setting title
set title
" Set title to vim /path/to/file
" Set titlelen to show full path
set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:~:.:h\")})%)%(\ %a%)\ \|\ vim titlelen=0

function CustomYamlIndent(lnum)
    " The main objective of this custom indent is to not indent lists, or comments preceding list
    " items. It does make another modification to the default indenting explained in the larger
    " comment below.
    if (a:lnum == 1)
        return 0
    endif

    let curr_line = getline(a:lnum)
    let prev_line = getline(a:lnum-1)
    let prev_line_ends_with_colon = (-1 != match(prev_line, ':\s*$'))
    let line_starts_with_dash = (0 == match(curr_line, '^\s*-'))
    let line_is_comment = (0 == match(curr_line, '^\s*\#'))

    if (line_starts_with_dash || line_is_comment)
        let prev_line_starts_with_dash = (0 == match(prev_line, '^\s*-'))
        let curr_line_is_child_list = prev_line_ends_with_colon && prev_line_starts_with_dash
        if (curr_line_is_child_list)
            return GetYAMLIndent(a:lnum)
        else
            let prev_line_indent = matchstr(prev_line, '^\s*')
            return strlen(prev_line_indent)
        endif
    else
        let prev_line_starts_with_dash = (0 == match(prev_line, '^\s*-'))
        if (prev_line_ends_with_colon && prev_line_starts_with_dash)
            " Here we override the default. The default does this:
            " - some:
            "   text
            " In other words, it assumes that a list item without a typed-in value is null. I
            " _think_ the more common use-case is that this list item is the beginning of an
            " object, so the override does this:
            " - some:
            "     text
            " Perhaps that will be annoying one day. We shall see.
            return GetYAMLIndent(a:lnum) + 2
        else
            " Use the default
            return GetYAMLIndent(a:lnum)
        endif
    endif
endfunction

" When opening a new file remember the cursor position of the last editing
if has("autocmd")
    " When editing a file, always jump to the last cursor position
    " autocmd BufReadPost * if line("'\"") | exe "'\"" && exe "zz" | endif
    " autocmd BufReadPost * if line("'\"") | exe "'\"" | exe "zz" | endif
    " autocmd BufReadPost * if line("'\"") | normal! "'\"zz" | endif
    " TODO: execute 'zz' after this to center the most-recent line
    autocmd BufReadPost * if line("'\"") | exe "'\"" | endif

    " TODO:
    " Does nvim support this yet?
    " augroup vimrc-incsearch-highlight
    "     autocmd!
    "     au CmdlineEnter /,\? :set hlsearch
    "     au CmdlineLeave /,\? :set nohlsearch
    " augroup END

    " http://vim.wikia.com/wiki/Indenting_source_code
    au FileType yaml setlocal shiftwidth=2 tabstop=2 sts=2 expandtab indentexpr=CustomYamlIndent(v:lnum)
    " au FileType yaml setlocal shiftwidth=2 tabstop=2 sts=2 expandtab
    au FileType json setlocal foldmethod=indent foldlevel=1
    au BufNewFile,BufRead */.dotfiles/notes/* setf markdown
    au BufNewFile,BufRead *.jsonl setf json
    au BufNewFile,BufRead *.boo setf boo
    au BufNewFile,BufRead *.erl,*.es.*.hrl,*.yaws,*.xrl set expandtab
    au BufNewFile,BufRead *.erl,*.es,*.hrl,*.yaws,*.xrl setf erlang
    au BufNewFile,BufRead *.Jenkinsfile,Jenkinsfile setf groovy
    au BufNewFile,BufRead *.service,*.timer setf systemd
    au BufNewFile,BufRead *.hs setl sw=2 sts=2 et
    au BufNewFile,BufRead *.md setl sw=2 sts=2 et
    au BufNewFile,BufRead *.java setl sw=2 sts=2 et
    au BufNewFile,BufRead *.go
                \ setl sw=4 noet ts=4 |
                \ exec "set listchars=tab:\\ \\ ,trail:\uB7"
endif

" Use non-default alg for diff
set diffopt+=internal,algorithm:patience

" Disable swap file
set noswapfile

set completeopt=menu,menuone,noselect

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

lua << EOF

------------------------------------------------------------------------------
-- COPIED FROM https://github.com/hrsh7th/nvim-cmp#recommended-configuration
-- with a little help from: https://github.com/neovim/nvim-lspconfig/wiki/Autocompletion#nvim-cmp
------------------------------------------------------------------------------

-- Setup nvim-cmp.
local cmp = require'cmp'

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  mapping = {
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
    ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
    ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
    ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    -- { name = 'vsnip' }, -- For vsnip users.
    { name = 'luasnip' }, -- For luasnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' }, -- For snippy users.
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()

------------------------------------------------------------------------------
-- COPIED FROM https://github.com/neovim/nvim-lspconfig#keybindings-and-completion
------------------------------------------------------------------------------

local nvim_lsp = require('lspconfig')

-- Use an langserver_on_attach function to only map the following keys
-- after the language server attaches to the current buffer
-- Note: you must pass the defined langserver_on_attach as an argument to every setup {} call and the
-- keybindings in langserver_on_attach only take effect on buffers with an active language server.
local langserver_on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'glf', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gld', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gli', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', 'glt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', 'glr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>lde', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '<space>ldp', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', '<space>ldn', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  -- buf_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>lf", "<cmd>lua vim.lsp.buf.format()<CR>", opts)
  buf_set_keymap('v', "<space>lf", "<cmd>lua vim.lsp.buf.format()<CR>", opts)

end

nvim_lsp.rust_analyzer.setup {
    capabilities = capabilities,
    on_attach = langserver_on_attach,
    flags = {
      debounce_text_changes = 150,
    },
    settings = {
        ["rust-analyzer"] = {
            cargo = {
                allFeatures = true
            }
        }
    }
}

nvim_lsp.pylsp.setup {
    capabilities = capabilities,
    on_attach = langserver_on_attach,
    flags = {
      debounce_text_changes = 150,
    },
    settings = {
        pylsp = {
            plugins = {
                pycodestyle = {
                    maxLineLength = 100
                }
            }
        }
    }
}

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- Note: do *not* configure this for Java, unless removing nvim-jdtls
local servers = { "zls", "yamlls", "hls", "tsserver", "rnix" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    capabilities = capabilities,
    on_attach = langserver_on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end


------------------------------------------------------------------------------
-- Copied from https://github.com/hoob3rt/lualine.nvim/blob/9726824f1dcc8907632bc7c32f9882f26340f815/README.md#usage-and-customization
------------------------------------------------------------------------------
require'lualine'.setup {
  options = {
    icons_enabled = true,
    theme = 'gruvbox',
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = {}
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
}

------------------------------------------------------------------------------
-- DERIVED FROM https://github.com/nvim-treesitter/nvim-treesitter#modules
------------------------------------------------------------------------------
-- TODO: perhaps more conventional to put parsers in ~/.local/share/nvim/site/parser/c.so
local treesitter_parser_install_dir = "/home/msk/.config/nvim/parser"
vim.opt.runtimepath:append(treesitter_parser_install_dir)
require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  -- Note that nix has treesitter parser packages which we install, so we install none here.
  ensure_installed = {},

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- Automatically install missing parsers when entering buffer
  auto_install = false,

  -- If you need to change the installation directory of the parsers (see -> Advanced Setup)
  -- It *looks* like this needs either
  --   1. to be in vim's runtime path, or
  --   2. vim.opt.runtimepath:append("/some/path/to/store/parsers")
  -- Note the parsers at this location are installed by nix. Installing parsers with nvim is
  -- likely to cause problems.
  parser_install_dir = treesitter_parser_install_dir,

  -- List of parsers to ignore installing
  ignore_install = {},

  indent = {
    enable = true;
  },

  -- Ref: https://github.com/nvim-treesitter/nvim-treesitter/tree/47ffd0dfc5500a7c48d1b4c2a01949f5d6a041f2#highlight
  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    disable = {
      "rust",        -- found highlighting too busy at the time of writing
      "dockerfile",  -- had an error at the time of writing
    },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },

  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  },
}

------------------------------------------------------------------------------
-- Copied from https://github.com/romgrk/nvim-treesitter-context/tree/b7d7aba81683c1cd76141e090ff335bb55332cba#configuration
------------------------------------------------------------------------------
require'treesitter-context'.setup{
    enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
    throttle = true, -- Throttles plugin updates (may improve performance)
    max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
    patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
        -- For all filetypes
        -- Note that setting an entry here replaces all other patterns for this entry.
        -- By setting the 'default' entry below, you can control which nodes you want to
        -- appear in the context window.
        default = {
            'class',
            'function',
            'method',
            -- 'for', -- These won't appear in the context
            -- 'while',
            -- 'if',
            -- 'switch',
            -- 'case',
        },
        -- Example for a specific filetype.
        -- If a pattern is missing, *open a PR* so everyone can benefit.
        --   rust = {
        --       'impl_item',
        --   },
    },
    exact_patterns = {
        -- Example for a specific filetype with Lua patterns
        -- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
        -- exactly match "impl_item" only)
        rust = true,
    }
}

------------------------------------------------------------------------------
-- Adapted from https://github.com/windwp/nvim-autopairs/tree/6b6e35fc9aca1030a74cc022220bc22ea6c5daf4#treesitter
------------------------------------------------------------------------------

-- https://github.com/windwp/nvim-autopairs/tree/aea913109d30c87df329ec9b8fea9aed6ef9f52a#treesitter
local npairs = require("nvim-autopairs")
local Rule = require('nvim-autopairs.rule')

npairs.setup({
    check_ts = true,
    ts_config = {
        lua = {'string'},-- it will not add a pair on that treesitter node
        javascript = {'template_string'},
    }
})

local ts_conds = require('nvim-autopairs.ts-conds')


-- press % => %% only while inside a comment or string
npairs.add_rules({
  Rule("%", "%", "lua")
    :with_pair(ts_conds.is_ts_node({'string','comment'})),
  Rule("$", "$", "lua")
    :with_pair(ts_conds.is_not_ts_node({'function'}))
})

------------------------------------------------------------------------------
-- Not copied from anywhere
------------------------------------------------------------------------------

-- Hop setup
-- Things Hop doesn't seem to do yet, that EasyMotion does:
-- - "until" motions, i.e. "t" motions; as opposed to "f" motions
-- - smartsign, i.e. let me press the "4" key to go to a "$" symbol

require'hop'.setup {
    keys = 'fjdksl',
    quit_key = 'q',
    perm_method = require'hop.perm'.TrieBacktrackFilling,
    case_insensitive = true,
    create_hl_autocmd = true,
    inclusive_jump = false, -- seems broken at the moment
    uppercase_labels = true,
}
-- Set up `f` as general hop hotkey to hint character
vim.api.nvim_set_keymap('x', 'f', "<cmd>lua require'hop'.hint_char2()<cr>", {})
vim.api.nvim_set_keymap('x', 't', "<cmd>lua require'hop'.hint_char2({ hint_offset = -1 })<cr>", {})
-- Set up actions in normal mode
local actions = { "", "d", "c", "<", ">", "y" }
for _, a in ipairs(actions) do
    vim.api.nvim_set_keymap('n', a .. 'f', a .. "<cmd>lua require'hop'.hint_char2()<cr>", {})
    vim.api.nvim_set_keymap('n', a .. 't', a .. "<cmd>lua require'hop'.hint_char2({ hint_offset = -1 })<cr>", {})
end

-- Adapted from https://github.com/lewis6991/gitsigns.nvim/tree/851cd32caaea84f303c4fdf51d72dfa5fcd795bb
require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  signcolumn = true,  -- Toggle with `:Gitsigns toggle_signs`
  numhl      = false, -- Toggle with `:Gitsigns toggle_numhl`
  linehl     = false, -- Toggle with `:Gitsigns toggle_linehl`
  word_diff  = false, -- Toggle with `:Gitsigns toggle_word_diff`
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
  attach_to_untracked = true,
  current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
    delay = 0,
    ignore_whitespace = false,
  },
  current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  max_file_length = 40000, -- Disable if file is longer than this (in lines)
  preview_config = {
    -- Options passed to nvim_open_win
    border = 'single',
    style = 'minimal',
    relative = 'cursor',
    row = 0,
    col = 1
  },
  yadm = {
    enable = false
  },
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map('n', ']c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(function() gs.next_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    map('n', '[c', function()
      if vim.wo.diff then return '[c' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    -- Text object
    map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')

    -- Hack together a gitsigns telescope picker
    -- TODO: put this in a separate file to limit its scope
    -- TODO: bundle as extension? https://github.com/nvim-telescope/telescope.nvim/blob/97847309cbffbb33e442f07b8877d20322a26922/developers.md#bundling-as-extension
    -- Derived from: https://github.com/nvim-telescope/telescope.nvim/blob/97847309cbffbb33e442f07b8877d20322a26922/developers.md
    local pickers = require "telescope.pickers"
    local finders = require "telescope.finders"
    local conf = require("telescope.config").values
    local actions = require "telescope.actions"
    local action_state = require "telescope.actions.state"

    local telescope_gitsigns = function(opts)
      opts = opts or {}
      pickers.new(opts, {
        prompt_title = "gitsigns options",
        finder = finders.new_table {
          results = {
            { "stage buffer",          gs.stage_buffer              },
            { "toggle line blame",     gs.toggle_current_line_blame },
            { "toggle deleted lines",  gs.toggle_deleted            },
            { "toggle line highlight", gs.toggle_linehl             },
            { "toggle word diff",      gs.toggle_word_diff          },
            { "reset hunk",            gs.reset_hunk                },
          },
          entry_maker = function(entry)
            return {
              value = entry,
              display = entry[1],
              ordinal = entry[1],
            }
          end
        },
        sorter = conf.generic_sorter(opts),
        attach_mappings = function(prompt_bufnr, map)
          actions.select_default:replace(function()
            actions.close(prompt_bufnr)
            local selection = action_state.get_selected_entry()
            selection.value[2]()
          end)
          return true
        end,
      }):find()
    end

    vim.keymap.set('n', '<leader>fg', telescope_gitsigns, {}) -- find gitsigns opts
    vim.keymap.set('v', '<leader>fg', telescope_gitsigns, {}) -- find gitsigns opts
  end
}

local telescope_builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', telescope_builtin.find_files, {}) -- find files
vim.keymap.set('n', '<leader>ft', telescope_builtin.live_grep, {})  -- find text
vim.keymap.set('n', '<leader>fb', telescope_builtin.buffers, {})    -- find buffers
vim.keymap.set('n', '<leader>fh', telescope_builtin.help_tags, {})  -- find help tags
-- TODO: does builtins include e.g. gitsigns? Probably not? What about extensions?
vim.keymap.set('n', '<leader>fa', telescope_builtin.builtin, {})    -- find builtins ("all")
vim.keymap.set('n', '<leader>fm', telescope_builtin.marks, {})      -- find marks
require('telescope').setup()
require('telescope').load_extension('fzy_native')

EOF

" TODO: make this work:
" Later note: possibly wasn't working because of the way I was setting colorscheme with
"   au VimEnter * colorscheme OceanicNext
highlight TreesitterContext ctermbg=white cterm=none

" TODO: what does ctrl+shift+f do? What is it bound to?

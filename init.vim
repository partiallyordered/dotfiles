
" TODO:
"  - Language server client
"  - Is it possible to enter a mode that maps the home row to numbers as follows:
"    [a s d f g h j k l ;] -> [1 2 3 4 5 6 7 8 9 0]
"    then a mode where these could be entered instead of numbers, which are easier to miss?
"    For example <leader>j would enter the mode, then 'as' would be equivalent to '10', such that
"    entering <leader>jasj would go down 10 lines, like 10j?
"    One problem would be that it wouldn't necessarily be easy to tell when number mode ended. An
"    end key or a fixed number of digits would be needed thus: <leader>j;sj (two digits two move
"    two lines) or <leader>jas<CR>j. The latter would defeat the purpose somewhat. So indeed might
"    the former- as being unusable in a subset of scenarios might diminish usefulness through
"    complexity in actual usage.
"  - Possible to display a solid bar at the wrap-around width to indicate visually when lines are
"    over that width?
"  - Often after an easymotion, it's desirable to enter insert mode immediately. Perhaps remap F to
"    do this? I.e. noremap F fXi (where X is the key in question)
"  - Show trailing tabs in the same manner as we show trailing spaces
"  - Using tcomment, take the <leader>tcy and add a number. E.g. <leader>tc4y to copy the current
"    line and the four following lines comment them, then paste.
"  - Set up ligatures for Haskell:
"    - http://www.bytesizedworkbench.com/blog/2017/10/24/the-joy-of-alacritty/<Paste>
"    - https://github.com/enomsg/vim-haskellConcealPlus
"  - Set up better documentation for Haskell
"  - Consider moving to spacemacs with Haskell layer for Haskell
"  - Set up a keybinding for visual mode to surround the current selection with a given
"    parenthesis. E.g., start with the selection: hello, end with the selection: (hello).

set nocompatible
filetype off

" Check Vundle is installed
" if !empty(glob('~/.vim/bundle/Vundle.vim'))
"    set rtp+=~/.vim/bundle/Vundle.vim
"    call vundle#begin()
"    " alternatively, pass a path where Vundle should install plugins
"    "call vundle#begin('~/some/path/here')
"
"    " let Vundle manage Vundle, required
"    Plugin 'VundleVim/Vundle.vim'
"
"    " nice
"    Plugin 'https://github.com/beigebrucewayne/Turtles'
"    " doesn't really work
"    Plugin 'https://github.com/hzchirs/vim-material'
"    " nice. Interestingly; only after having a dark colorscheme (perhaps only solarized?)
"    Plugin 'https://github.com/rakr/vim-one'
"    " meh
"    Plugin 'https://github.com/bluz71/vim-moonfly-colors'
"    " ok
"    Plugin 'https://github.com/lu-ren/SerialExperimentsLain'
"    " like a duller solarized
"    Plugin 'https://github.com/nightsense/vim-crunchbang'
"    " basic-dark is ok
"    Plugin 'https://github.com/zcodes/vim-colors-basic'
"    " doesn't really work
"    Plugin 'https://github.com/tyrannicaltoucan/vim-deep-space'
"    " doesn't work (24 bit??)
"    Plugin 'https://github.com/carakan/new-railscasts-theme'
"    " a bit basic
"    Plugin 'https://github.com/danilo-augusto/vim-afterglow'
"    " a bit basic
"    Plugin 'https://github.com/rhysd/vim-color-spring-night'
"    " a bit nicer than monokai
"    Plugin 'https://github.com/reewr/vim-monokai-phoenix'
"    " a bit basic
"    Plugin 'https://github.com/ltlollo/diokai'
"    " doesn't show up colours well (24 bit??)
"    Plugin 'https://github.com/colepeters/spacemacs-theme.vim'
"    " meh
"    Plugin 'https://github.com/crusoexia/vim-monokai'
"    " meh
"    Plugin 'https://github.com/tomasr/molokai'
"
"    Plugin 'https://github.com/mxw/vim-jsx'
"    Plugin 'https://github.com/LnL7/vim-nix'
"    Plugin 'https://github.com/altercation/vim-colors-solarized'
"    Plugin 'https://github.com/jparise/vim-graphql'
"    Plugin 'https://github.com/guns/vim-clojure-static'
"    Plugin 'https://github.com/AndrewRadev/sideways.vim'
"    Plugin 'https://github.com/cespare/vim-toml'
"    Plugin 'https://github.com/easymotion/vim-easymotion'
"    Plugin 'https://github.com/haya14busa/incsearch.vim'
"    " Plugin 'https://github.com/itchyny/vim-haskell-indent'
"    Plugin 'https://github.com/neovimhaskell/haskell-vim'
"    Plugin 'https://github.com/leafgarland/typescript-vim'
"    Plugin 'https://github.com/michaeljsmith/vim-indent-object'
"    Plugin 'https://github.com/msteinert/vim-ragel'
"    Plugin 'https://github.com/pangloss/vim-javascript'
"    Plugin 'https://github.com/Raimondi/delimitMate/'
"    Plugin 'https://github.com/rust-lang/rust.vim'
"    Plugin 'https://github.com/SirVer/ultisnips'
"    Plugin 'https://github.com/tomtom/tcomment_vim'
"    Plugin 'https://github.com/tpope/vim-repeat'
"    Plugin 'https://github.com/tpope/vim-surround'
"    Plugin 'https://github.com/kana/vim-textobj-user'
"    Plugin 'dracula/vim'
"    Plugin 'elixir-lang/vim-elixir'
"    " TODO: this doesn't seem to recognise multiple single-line javascript comments (or perhaps
"    " single-line javascript comments at all). PR?
"    " OR: maybe it doesn't work with the 'ic' (i.e. 'in comment') object. Might just be best to get
"    " used to using 'ac' (i.e. 'around comment') object.
"    Plugin 'https://github.com/glts/vim-textobj-comment'
"
"    " All of your Plugins must be added before the following line
"    call vundle#end()            " required
" endif
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" From: https://wiki.archlinux.org/index.php/vim/.vimrc
" .vimrc
" See: http://vimdoc.sourceforge.net/htmldoc/options.html for details

" For multi-byte character support (CJK support, for example):
"set fileencodings=ucs-bom,utf-8,cp936,big5,euc-jp,euc-kr,gb18030,latin1

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

set timeoutlen=300  " Default is 1000; which is a long time
                    " An idea for managing this if the timeoutlen is too short:
                    " https://stackoverflow.com/questions/26829086/key-specific-timeoutlen-in-vim

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

" Solarized options
" TODO: can these settings be moved to ~/.vim/after/plugins/ ?
" TODO: need to check for solarized, otherwise an annoying message is shown
" when it doesn't exist. (Or is that better?)
syntax on
" set t_Co=16
au VimEnter * colorscheme OceanicNext
" au VimEnter * colorscheme molokai
set background=dark " When set to "dark", Vim will try to use colors that look
                    " good on a dark background. When set to "light", Vim will
                    " try to use colors that look good on a light background.
                    " Any other value is illegal.

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
xnoremap <leader>sort :!sort<CR>
xnoremap <leader>nsort :!sort -n<CR>
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

" C specific (perhaps project specific in places)
nnoremap <leader>blk O#if 0<ESC>jo#endif<ESC>k0
nnoremap <leader>ublk ?#if 0<CR>dd/#endif<CR>dd
" nnoremap <leader>com :set lz<CR>^i/*<ESC>:call search('.\/\*\\|$')<CR>a*/<ESC>j^:noh<CR>:set nolz<CR>
" nnoremap <leader>ucom :set lz<CR>k$:call search('\/\*')<CR>2x:call search('\*\/')<CR>2xj^:noh<CR>:set nolz<CR>
" nnoremap <leader>fdec <ESC>:set lz<CR>?^{<CR>?[^ \(\)]\+\s*(.*$<CR>ye/{<CR>oDEBUG("<ESC>pA start");<CR><ESC>kk%O<CR>DEBUG("<ESC>pA end");<ESC>3<C-O>:noh<CR>:set nolz<CR>
nnoremap <leader>fdec :set lz<CR>?^{<CR>oTRACE();<ESC>k0%?^\(\(.*return.*\)\@!.\)*$<CR>oTRACEEND();<ESC>:noh<CR>:set nolz<CR>
nnoremap <leader>cdec <ESC>:set ls<CR>$?case .*:\s*\(\/\*.*\*\/\)*\s*$<CR>wyeoDEBUG("<ESC>pA");<ESC><C-O>:noh<CR>:set nolz<CR>
" Can't handle repeated presses of <leader>,
" nnoremap <leader>, <<
" nnoremap <leader>. >>

" TODO: see the sections about using syntax space errors and showing long lines here:
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
" Show trailing spaces at the end of a line. Show tabs.
exec "set listchars=trail:\uB7,tab:\uBB\uBB"
set list

" Highlight column 81
" call matchadd('ColorColumn', '\%81v', 100)

" TODO: move to ~/.vim/after/plugins/ ?
" UltiSnips options
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"

" See https://github.com/pangloss/vim-javascript for other conceal options
" let g:javascript_conceal_arrow_function = "⇒"

" TODO: move to ~/.vim/after/plugins/ (probably not, as these variables may
" need to be set before ycm is loaded, rather than after) ?
" Is there a .vim/before/plugins?
" YCM options
let g:ycm_server_python_interpreter = '/usr/bin/python2'
let g:ycm_use_ultisnips_completer = 1
let g:ycm_collect_identifiers_from_tags_files=1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_confirm_extra_conf = 1
let g:ycm_add_preview_to_completeopt = 1
" The following line whitelists ycm config files
let g:ycm_extra_conf_globlist = [
            \ '/home/msk/projects/bson_processor/.ycm_extra_conf.py',
            \ '~/.ycm_extra_conf.py']

let g:clojure_fuzzy_indent=1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

let g:dart_style_guide = 2
let g:dart_format_on_save = 1

" TODO: still using vim-surround? Delete this stuff?
" vim-surround options
" lower-case b to surround with ()
let g:surround_97 = "(\r)"
" upper-case B to surround with {}
let g:surround_66 = "{\r}"
" lower-case s to surround with []
let g:surround_115 = "[\r]"
" switch bracket functionality so closing and opening brackets do and do not add space respectively
let g:surround_40 = "(\r)"
let g:surround_41 = "( \r )"
let g:surround_91 = "[\r]"
let g:surround_93 = "[ \r ]"
let g:surround_123 = "{\r}"
let g:surround_125 = "{ \r }"


" markdown-preview settings
let g:mkdp_auto_start = 1
let g:mkdp_browser = 'chromium'

" vim-gh-line open command
let g:gh_open_command = 'fn() { echo "$@" | xclip -i -sel p -f | xclip -i -sel s -f | xclip -i -sel c; }; fn'

" vim-autoformat options
let g:formatdef_custom_sql = '"sqlformat --comma-first true --reindent_aligned -k upper --indent_after_first"'
let g:formatters_sql = ['custom_sql']

" ALE options
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_lint_on_text_changed = 'never'

" Enable setting title
set title
" Set title to vim /path/to/file
" Set titlelen to show full path
set titlestring=vim\ %F titlelen=0

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
    au FileType yaml setlocal shiftwidth=2 tabstop=2 sts=2 expandtab
    au FileType json setlocal foldmethod=indent foldlevel=1
    au BufNewFile,BufRead */.dotfiles/notes/* setf markdown
    au BufNewFile,BufRead *.boo setf boo
    au BufNewFile,BufRead *.erl,*.es.*.hrl,*.yaws,*.xrl set expandtab
    au BufNewFile,BufRead *.erl,*.es,*.hrl,*.yaws,*.xrl setf erlang
    au BufNewFile,BufRead *.Jenkinsfile,Jenkinsfile setf groovy
    au BufNewFile,BufRead *.service,*.timer setf systemd
    au BufNewFile,BufRead *.hs setl sw=2 sts=2 et
    au BufNewFile,BufRead *.md setl sw=2 sts=2 et
    au BufNewFile,BufRead *.go
                \ setl sw=4 noet ts=4 |
                \ exec "set listchars=tab:\\ \\ ,trail:\uB7"
endif

" Use non-default alg for diff
set diffopt+=internal,algorithm:patience

" Disable swap file
set noswapfile

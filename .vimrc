
" TODO:
"  - Is it possible to enter a mode that maps the home row to numbers as follows:
"    [a s d f g h j k l ;] -> [1 2 3 4 5 6 7 8 9 0]
"    then a mode where these could be entered instead of numbers, which are easier to miss?
"    For example <leader>j would enter the mode, then as would be equivalent to '10', such that
"    entering <leader>jasj would go down 10 lines, like 10j?
"    One problem would be that it wouldn't necessarily be easy to tell when number mode ended. An
"    end key or a fixed number of digits would be needed thus: <leader>j;sj (two digits two move
"    two lines) or <leader>jas<CR>j. The latter would defeat the purpose somewhat. So indeed might
"    the former- as being unusable in a subset of scenarios might diminish usefulness through
"    complexity in actual usage.

set nocompatible
filetype off

" Check Vundle is installed
if !empty(glob('~/.vim/bundle/Vundle.vim'))
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
    " alternatively, pass a path where Vundle should install plugins
    "call vundle#begin('~/some/path/here')

    " let Vundle manage Vundle, required
    Plugin 'VundleVim/Vundle.vim'

    Plugin 'https://github.com/guns/vim-clojure-static'
    Plugin 'https://github.com/altercation/vim-colors-solarized'
    Plugin 'https://github.com/AndrewRadev/sideways.vim'
    Plugin 'https://github.com/cespare/vim-toml'
    Plugin 'https://github.com/easymotion/vim-easymotion'
    Plugin 'https://github.com/haya14busa/incsearch.vim'
    Plugin 'https://github.com/itchyny/vim-haskell-indent'
    Plugin 'https://github.com/leafgarland/typescript-vim'
    Plugin 'https://github.com/michaeljsmith/vim-indent-object'
    Plugin 'https://github.com/msteinert/vim-ragel'
    Plugin 'https://github.com/pangloss/vim-javascript'
    Plugin 'https://github.com/Raimondi/delimitMate/'
    Plugin 'https://github.com/rust-lang/rust.vim'
    Plugin 'https://github.com/SirVer/ultisnips'
    Plugin 'https://github.com/tomtom/tcomment_vim'
    Plugin 'https://github.com/tpope/vim-repeat'
    Plugin 'https://github.com/tpope/vim-surround'
    Plugin 'https://github.com/kana/vim-textobj-user'
    Plugin 'dracula/vim'
    Plugin 'elixir-lang/vim-elixir'
    " TODO: this doesn't seem to recognise multiple single-line javascript comments (or perhaps
    " single-line javascript comments at all). PR?
    " OR: maybe it doesn't work with the 'ic' (i.e. 'in comment') object. Might just be best to get
    " used to using 'ac' (i.e. 'around comment') object.
    Plugin 'https://github.com/glts/vim-textobj-comment'

    " All of your Plugins must be added before the following line
    call vundle#end()            " required
endif
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

set relativenumber  " Show relative line numbers, except on the current line,
set number          " which will show the current line number

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

set formatoptions=c,q,r,t " This is a sequence of letters which describes how
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

set formatoptions-=o " do not automatically insert comments when pressing o/O
                    " inside comments

set ruler           " Show the line and column number of the cursor position,
                    " separated by a comma.
set mouse=a         " Enable the use of the mouse.

set laststatus=2
set tags=./tags;
" Use unnamedplus to set the clipboard to the + (shared between vim sessions)
" register
set clipboard=unnamed

" Incrementally change substituted text when using ':%s//'
set inccommand=split

"Make history persistent:
set history=500
set viminfo=%,'20,/100,:100,<500,f1,n~/.vim/viminfo

filetype off

" Allow use of <C-Q> and <C-S> by stopping the TTY from capturing them
" silent !stty -ixon > /dev/null 2>/dev/null
" This doesn't work..
" au VimLeave * !stty ixon > /dev/null 2>/dev/null

if has('nvim')
    let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
    let g:solarized_italic=0
    let g:solarized_termcolors=16
    let g:solarized_style="dark"
    let g:solarized_contrast="high"
    set rtp^=/usr/share/vim/vimfiles/
endif

" Do this after adding the old vimfiles to rtp; else plugins may not work
" correctly
filetype plugin indent on

" Solarized options
" TODO: can these settings be moved to ~/.vim/after/plugins/ ?
" TODO: need to check for solarized, otherwise an annoying message is shown
" when it doesn't exist. (Or is that better?)
syntax on
set t_Co=16
au VimEnter * colorscheme solarized
" au VimEnter * colorscheme dracula
set background=dark " When set to "dark", Vim will try to use colors that look
                    " good on a dark background. When set to "light", Vim will
                    " try to use colors that look good on a light background.
                    " Any other value is illegal.

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
nnoremap <leader>rs :%s/\<<C-R><C-W>\>/
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

" TODO: move to ~/.vim/after/plugins/ (probably not, as these variables may
" need to be set before ycm is loaded, rather than after) ?
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

autocmd FileType yaml setlocal shiftwidth=2 tabstop=2

" Enable setting title
set title
" Set title to vim /path/to/file
" Set titlelen to show full path
set titlestring=vim\ %F titlelen=0

" When opening a new file remember the cursor position of the last editing
if has("autocmd")
    " When editing a file, always jump to the last cursor position
    autocmd BufReadPost * if line("'\"") | exe "'\"" | endif

    au BufNewFile,BufRead *.boo setf boo
    autocmd BufRead,BufNewFile *.erl,*.es.*.hrl,*.yaws,*.xrl set expandtab
    au BufNewFile,BufRead *.erl,*.es,*.hrl,*.yaws,*.xrl setf erlang
    au BufNewFile,BufRead *.Jenkinsfile,Jenkinsfile setf groovy
    au BufNewFile,BufRead *.service,*.timer setf systemd
    au FileType haskell setlocal shiftwidth=2 tabstop=2
endif

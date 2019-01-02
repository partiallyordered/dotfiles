# TODO: when entering reverse-history-search (<C-R>) and the key chord fd is set to exit insert
# mode, pressing f exits reverse-history-search mode. Is it possible to prevent this? Perhaps every
# time reverse-history-search mode is entered we can map fd to <nop> (or whatever no-op is called)
# and whenever reverse-history-search mode is exited we can remap fd to exit insert mode.
# TODO: store command history in sqlite3 db:
# https://github.com/larkery/zsh-histdb
# https://news.ycombinator.com/item?id=15041772
# https://github.com/barabo/advanced-shell-history
# https://stackoverflow.com/questions/17417190/logging-bash-history-in-a-database
# https://www.reddit.com/r/zsh/comments/67gsm8/a_thing_i_made_to_put_your_zsh_history_into_a/
# TODO: use a key binding to print the dirstack and allow selection of a directory to change to
# TODO: is it possible to replace all relative filenames provided on the command-line with
# absolute filenames? This way it would be easier to reopen a file previously opened, even if the
# working directory has been changed. E.g.:
#   vim fstab
#   cd ..
#   <Up><Up> # "vim fstab" no longer opens fstab
# Compare:
#   vim fstab # replaced automatically with vim /etc/fstab
#   cd ..
#   <Up><Up> # "vim /etc/fstab" still opens the same file
# This could maybe be accomplished with some sort of zsh-expand-[something] function.
# TODO: show git status on files in ls
# TODO: show hidden files/directories in autocomplete

# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify %d'
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix
zstyle ':completion:*' file-sort name
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd .. directory
zstyle ':completion:*' insert-unambiguous false
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' original false
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
zstyle ':completion:*' regular always # always expand aliases, even when not in command position
zstyle :compinstall filename '/home/matt/.zshrc'

autoload edit-command-line
zle -N edit-command-line
autoload -Uz compinit && compinit
autoload -U colors && colors
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd extendedglob nomatch notify autopushd pushdsilent \
    pushdtohome pushdminus pushdignoredups completealiases interactivecomments
unsetopt beep
# 10 gives us enough time to use the key chord fd to exit insert mode
export KEYTIMEOUT=10
# End of lines configured by zsh-newuser-install

# bash autocomplete
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"

# Stop ssh autocomplete from taking ages
zstyle ':completion:*' hosts off

# If the following is required, probably cache at startup it in ~/.cache or a temp dir or something
# python2_site_pkgs_dir=$(python2 -c 'from distutils.sysconfig import get_python_lib; print get_python_lib()')
# python_site_pkgs_dir=$(python -c 'from distutils.sysconfig import get_python_lib; print(get_python_lib())')

# ANDROID_HOME="~$HOME/"
# https://stackoverflow.com/a/44386974
# ANDROID_EMULATOR_USE_SYSTEM_LIBS=1 
# https://stackoverflow.com/a/44931873
export \
    TERMCMD="alacritty" \
    ANDROID_HOME="$HOME/.android-sdk/" \
    ANDROID_EMULATOR_USE_SYSTEM_LIBS=1 \
    GOPATH="/proj/go" \
    MINIKUBE_HOME="/mnt/virtualisation" \
    EDITOR="vim" \
    N_PREFIX="$HOME/bin/" \
    PATH="$HOME/.cargo/bin:$PATH"
    PATH="/opt/clojurescript/bin:$HOME/.node_modules/bin:$HOME/.npm-packages/bin:$PATH"
    PATH="$HOME/bin/bin/:$HOME/bin:$HOME/.node_modules/bin:$python_site_pkgs_dir:$PATH"
    PATH="$HOME/bin/flutter/bin:$GOPATH/bin/:$HOME/.local/bin/:$HOME/.cabal/bin/:$PATH"

PROMPT="%{$fg_no_bold[white]%}%n%{$fg_no_bold[yellow]%}|%{$fg_no_bold[white]%}%m %{$fg_no_bold[red]%}%?%{$fg_no_bold[yellow]%} # "
RPROMPT="%{$fg_no_bold[white]%}%d%{$fg_no_bold[yellow]%}|%{$fg_no_bold[white]%}%T%{$reset_color%}"

# Don't alias iptables as this interferes with other iptables functionality
alias less="less -R" # colorise
alias iptablesl="sudo iptables --line-numbers -nvL"
alias ag="ag --hidden --path-to-ignore ~/.ignore"
alias sag="sag --hidden --path-to-ignore ~/.ignore"
alias netstat="netstat -plunt"
alias grep="grep --color=auto"
alias ts="grep --exclude-dir=\".svn\" --exclude-dir=\".git\" -IR -m 1"
alias tsa="grep --exclude-dir=\".svn\" --exclude-dir=\".git\" -IR"
alias terman="$TERM man"
alias dir="dir --color=auto"
alias rsync="rsync -r --progress"
alias strace="strace -v -s 100000"
alias feh="feh -F"
alias fehh="feh --info 'echo \"\$(ls \"\$(dirname %F)\" | wc -l) \$(du -s %F | cut -f1) \$(basename %F)\"'"
alias vt="v -t"
alias tag="v -t"
alias mountl="mount | column -t"
alias chmox="chmod +x"
alias vimls="^ls^v"
alias gdt="git difftool"
alias gst="git status"
alias glg="git log"
alias gcm="git commit -m"
alias gcw="git commit -m \"whatever\""
alias gaa="git add -A"
alias gau="git add -u"
alias v="nvim"
alias vd="vimdiff"
alias svnds="svn diff --summarize"
alias sc="systemctl"
alias scs="systemctl status"
alias scrs="systemctl restart"
# alias nv="nvim"
alias vim="nvim"
# function use_v_you_clown() {
#     echo "Use v you clown. What were you thinking trying to open $@ with 'vim'?!"
# }
# alias vim=use_v_you_clown
alias e="emacsclient -t"
alias cp="cp --reflink=auto" # enables instant 'copy' in supporting file systems, e.g. btrfs
alias dc="docker-compose"

alias -g pg="| egrep"

# Open the following file suffixes in vim
alias -s cpp=vim
alias -s hpp=vim
alias -s c=vim
alias -s h=vim
alias -s hs=vim
alias -s go=vim
alias -s yaml=vim
alias -s json=vim
alias -s js=vim
# Open Makefiles in vim
alias Makefile="vim Makefile"
alias makefile="vim makefile"

# =================================================================================
# From https://wiki.archlinux.org/index.php/Zsh#Key_bindings
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}

key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"     ]]  && bindkey  "${key[Home]}"     beginning-of-line
[[ -n "${key[End]}"      ]]  && bindkey  "${key[End]}"      end-of-line
[[ -n "${key[Insert]}"   ]]  && bindkey  "${key[Insert]}"   overwrite-mode
[[ -n "${key[Delete]}"   ]]  && bindkey  "${key[Delete]}"   delete-char
[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
[[ -n "${key[Left]}"     ]]  && bindkey  "${key[Left]}"     backward-char
[[ -n "${key[Right]}"    ]]  && bindkey  "${key[Right]}"    forward-char
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"   beginning-of-buffer-or-history
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}" end-of-buffer-or-history

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        printf '%s' "${terminfo[smkx]}"
    }
    function zle-line-finish () {
        printf '%s' "${terminfo[rmkx]}"
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

# =================================================================================

mkcd() {
    mkdir -p $1
    cd $1
}

up_dir() {
    num_of_dirs=$1
    cd_dir=$PWD
    for i in $(seq 1 $num_of_dirs)
    do
        cd_dir=$(dirname $cd_dir)
    done
    cd $cd_dir
}
alias up=up_dir

down_dir() {
    parent_dir=$PWD
    cd -
    if [[ $PWD/ == $parent_dir/* ]]; then
        subdir_str=${PWD/$parent_dir/}
        subdir_depth=${#subdir_str//[^\/]/}
        if (($# == 0)); then
            down_num=$((subdir_depth - 1))
        else
            down_num=$((subdir_depth - $1))
        fi;
        up_dir "$down_num"
    else
        cd -
        print "Previous directory is not subdirectory of current directory"
    fi;
}
alias dn=down_dir

# findinfiles() {
#     find . -type f \
#         -not -iwholename '*.so' \
#         -not -iwholename '*.a' \
#         -not -iwholename '*.lib' \
#         -not -iwholename '*.dll' \
#         -not -name 'tags' \
#         -not -path '*.git*' \
#         -not -path '*.svn*' \
#         -not -path '*.swp*' \
#         -not -iwholename '*.o' \
#         -not -iwholename '*.d' | xargs -I{} egrep -l "$1" {}
#     return 0;
# }

# vimfindinfiles() {
#     vim +/"$1" $(findinfiles "$1")
#     return 0;
# }
# alias vf=vimfindinfiles

vimtextsearch() {
    results=$(ts -l "$1")
    echo "$results"
    v +/"$1" $(echo "$results" | tr '\n' ' ')
}
alias vts=vimtextsearch

DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi;

chpwd() {
  print -l $PWD ${(u)dirstack} > $DIRSTACKFILE
  DIRSTACKSIZE=20
}

# Exists as demonstration of extraction of last argument to function.
# lastarg() {
#     last_arg=${@[-1]}
#     rest=${@:1:$(($# - 1 ))}
# }

symsearch() {
    # Just wraps textsearchall search patterns with a word boundary to make it
    # easy to search for a symbol.
    search_term=${@[-1]}
    arg_length=$(($# - 1))
    pass_args=${@:1:$arg_length}
    tsa $pass_args "\b$search_term\b"
}
alias sss=symsearch

vimsymsearch() {
    local FILES=$(ts -l "\b$1\b")
    echo "$FILES"
    v +/"\<$1\>" $(echo "$FILES" | tr '\n' ' ')
}
alias vss=vimsymsearch

vimsymsearchlast() {
    # Get the last command.
    last_cmd=$(fc -ln -1) # zsh only, I think
    # Split it into an array on spaces.
    last_cmd=(${(s: :)last_cmd})
    # Get the length
    last_cmd_len=$(($#last_cmd - 1))
    # Get the last item of the last command.
    last_cmd_search_arg=${last_cmd:$last_cmd_len}
    # Pass to vss
    vimsymsearch "$last_cmd_search_arg"
}
alias vssl=vimsymsearchlast

svnvimdiff() {
    FILE_NAME=$1:t # :t is a zsh modifier and takes the tail (i.e. just the filename)
    TEMP_DIFF_FILE=$(mktemp "$FILE_NAME.BASE.tmp.XXX")
    svn cat -r BASE "$1" > $TEMP_DIFF_FILE
    vimdiff $TEMP_DIFF_FILE "$1"
    rm $TEMP_DIFF_FILE
    unset TEMP_DIFF_FILE FILE_NAME
}
alias svd=svnvimdiff

svnvimdiffbetween() {
    REV1=$1
    REV2=$2
    FILE_NAME=$3:t # :t is a zsh modifier and takes the tail (i.e. just the filename)
    REV1_TMP_DIFF_FILE=$(mktemp "$FILE_NAME.$REV1.tmp.XXX")
    REV2_TMP_DIFF_FILE=$(mktemp "$FILE_NAME.$REV2.tmp.XXX")
    svn cat -r "$REV1" "$3" > $REV1_TMP_DIFF_FILE
    svn cat -r "$REV2" "$3" > $REV2_TMP_DIFF_FILE
    vimdiff "$REV1_TMP_DIFF_FILE" "$REV2_TMP_DIFF_FILE"
    rm "$REV1_TMP_DIFF_FILE" "$REV2_TMP_DIFF_FILE"
    unset REV1 REV2 FILE_NAME REV1_TMP_DIFF_FILE REV2_TMP_DIFF_FILE
}
alias svdb=svnvimdiffbetween

svnvimdiffall() {
    # Make a warning indicating how many files there are and whether the user
    # would like to proceed.
    ANS=''
    FLINES=( "${(f)$(svn st pg "^M" | sed -e s/\^M\\s\*//g)}" )
    # NUM_FLINES_PAR="There are ${#FLINES} files to compare. Continue? (Y/n/l): "
    # vared -p $NUM_FLINES_PAR ANS
    for ITEM in $FLINES
    do
        ANS=''
        vared -p "Diff $ITEM? (y/n/q) " ANS
        if [[ $ANS == 'y' || $ANS == 'Y' ]]; then
            svnvimdiff $ITEM
        elif [[ $ANS == 'q' ]]; then
            break;
        fi;
    done
    # if [[ $ANS == 'y' || $ANS == 'Y' ]]; then
    #     for ITEM in $FLINES; svnvimdiff $ITEM
    # elif [[ $ANS == 'l' ]]; then
    #     for ITEM in $FLINES; echo "$ITEM"
    #     svnvimdiffall
    # fi;
    unset ANS FLINES NUM_FLINES_PAR
}
alias svda=svnvimdiffall

stopwatch() {
   date1=`date +%s`; while true; do
       echo -ne "\r$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)";
       sleep 0.01;
   done
}

function countdown(){
    date1=$((`date +%s` + $1));
    while [ "$date1" -ne `date +%s` ]; do
        echo -ne "\r$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)";
        sleep 0.01;
    done
}

function findheader(){
    # echo "#include \"$1\""       Create a fake c file
    # gcc -x c -M -                Generate a make rule as language c
    # sed "s/\([^\\\]\) /\1\n/g"   Split new lines on unescaped spaces
    # grep "$1"                    Find our argument in the output
    # awk '$1=$1'                  Remove leading/trailing spaces
    echo "#include \"$1\"" | gcc -x c -M - | sed "s/\([^\\\]\) /\1\n/g" | grep "$1" | awk '$1=$1'
}

# Change to the directory containing a given file
function cdf() {
    if [[ -f "$1" ]]; then
        builtin cd $(dirname "$1")
    else
        builtin cd "$1"
    fi;
}
function cdrf() {
    cdf **/$1
}
# alias cd=cdf

# Change to the first subdirectory of the working directory listed in alphabetical order
function cd1(){
    cd $(find $PWD -maxdepth 1 -mindepth 1 -type d | sort | head -n 1)
}

function cl(){
    cd $1; ls
}

# Change the the last subdirectory of the working directory listed in alphabetical order
function cdl(){
    cd $(find $PWD -maxdepth 1 -mindepth 1 -type d | sort | tail -n 1)
}

function intconv() {
    echo "obase=$1;ibase=$2;$3" | bc
}

function h2d() {
    if (($# != 0)); then
        intconv 10 16 $1
    fi;
}

function d2h() {
    if (($# != 0)); then
        intconv 16 10 $1
    fi;
}

function mkcdt() {
    cd $(mktemp -d)
}

function mkscratch() {
    mkdir -p "/proj/scratch/$1"
    cd "/proj/scratch/$1"
}

my_ls () {
    \ls -hAl --color=auto "$@"
    # /usr/bin/ls -hAl --color=auto "$@"
}

# Automatically ls on empty line
auto-ls () {
    if [[ $#BUFFER -eq 0 ]]; then
        echo ""
        my_ls
        zle redisplay
    else
        zle .$WIDGET
    fi
}
zle -N accept-line auto-ls
zle -N other-widget auto-ls

vi-cmd-up-line-history() {
  zle vi-cmd-mode
  zle up-line-or-history
}
zle -N vi-cmd-up-line-history

# TODO: any reverse history search containing an 'f' does not work..
# 'fd' to exit insert, reverse-search mode
# https://unix.stackexchange.com/questions/63353/is-there-a-command-for-switch-to-vicmd-mode-in-zsh
bindkey -M viins 'fd' vi-cmd-mode
bindkey -M isearch 'fd' vi-cmd-up-line-history

# Change cursor to bar when in zsh 'insert mode'
zle-keymap-select () {
    # Alacritty advertises itself as xterm; this works
    if [[ $TERM = "rxvt-unicode-256color" || $TERM = "xterm-256color" || $TERM = "alacritty" ]]; then
        if [ $KEYMAP = vicmd ]; then
            echo -ne "\033[2 q"
        else
            echo -ne "\033[6 q"
        fi
    fi
}
zle -N zle-keymap-select
zle-line-init () {
    zle -K viins
    if [ $TERM = "rxvt-unicode-256color" ]; then
        echo -ne "\033[6 q"
    fi
}
zle-line-finish () {
    # The advantage of this is that the cursor won't be a bar when starting emacs
    zle -K viins
    if [ $TERM = "rxvt-unicode-256color" ]; then
        echo -ne "\033[2 q"
    fi
}
zle -N zle-line-init
bindkey -v

# Bind <C-R> to incremental search like normal
# bindkey -M vicmd '^r' history-incremental-pattern-search-backward
bindkey -M viins '^r' history-incremental-pattern-search-backward

bindkey -M vicmd '^r' redo
bindkey -M vicmd 'u' undo
bindkey -M viins "${key[Home]}" beginning-of-line
bindkey -M vicmd "${key[Home]}" beginning-of-line
bindkey -M viins "${key[End]}" end-of-line
bindkey -M vicmd "${key[End]}" end-of-line
bindkey -M viins "${key[Delete]}" delete-char
bindkey -M vicmd "^e" edit-command-line
bindkey -M viins '^e' edit-command-line

alias cwd="echo -n $PWD | xclip"

# Duplicate command to window name
case $TERM in
    rxvt*|alacritty)
        # From: http://stackoverflow.com/questions/20727730/dynamic-window-title-in-urxvt-with-zsh
        # Write some info to terminal title.
        # This is seen when the shell prompts for input.
        function precmd {
            print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
        }
        # Write command, args, working directory to terminal title.
        # This is seen while the shell waits for a command to complete.
        function preexec {
            printf "\033]0;zsh | %s [%s]\a" "$1" "$PWD"
        }
        ;;
esac

ls_fn () {
    if [ "$#" -eq 0 ]; then
        echo "nope"
    else
        my_ls "$@"
    fi
}
alias ls="ls_fn"

# Small utility for making/searching notes/snippets
tv () {
    root="$HOME/.dotfiles/notes/"
    res="$(find $root -type f -printf '%P\n' | fzy)"
    if [[ ! -z "$res" ]]; then
        vim "$root/$res"
    fi
}

# https://blog.patshead.com/2012/11/automatically-expaning-zsh-global-aliases---simplified.html
globalias() {
    # exclude cd alias, it's __enhancd::cd from the enhancd package
    if [[ $LBUFFER != 'cd' ]]; then
        zle _expand_alias
        zle expand-word
    fi
    zle self-insert
}
zle -N globalias

# TODO: expand aliases the same in isearch?
# space, semicolon expands all aliases, including global
bindkey -M emacs " " globalias ";" globalias
bindkey -M viins " " globalias ";" globalias

# control-space to make a normal space
bindkey -M emacs "^;" magic-space "^ " magic-space
bindkey -M viins "^;" magic-space "^ " magic-space

# normal space during searches
bindkey -M isearch " " magic-space ";" magic-space

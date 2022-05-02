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
# zstyle ':completion:*' regular always # always expand aliases, even when not in command position
zstyle :compinstall filename '/home/matt/.zshrc'

autoload edit-command-line
zle -N edit-command-line
autoload -Uz compinit && compinit
autoload -U colors && colors
autoload -Uz vcs_info
# TODO:
# - show both upstream AND branch
# - display upstream more loudly if it's not 'origin'?
# - display branch more loudly if it's not 'master'?
zstyle ':vcs_info:git:*' formats 'on branch %b'
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd extendedglob nomatch notify autopushd pushdsilent \
    pushdtohome pushdminus pushdignoredups interactivecomments
unsetopt beep
# 10 gives us enough time to use the key chord fd to exit insert mode
export KEYTIMEOUT=10
# End of lines configured by zsh-newuser-install

# bash autocomplete
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"

# Stop ssh autocomplete from taking ages
zstyle ':completion:*' hosts off

PROMPT="%{$fg_no_bold[white]%}%n%{$fg_no_bold[yellow]%}|%{$fg_no_bold[white]%}%m %{$fg_no_bold[red]%}%?%{$fg_no_bold[yellow]%} ${vcs_info_msg_0_} $ "
RPROMPT="%{$fg_no_bold[white]%}%d%{$fg_no_bold[yellow]%}|%{$fg_no_bold[white]%}%T%{$reset_color%}"

alias ls="ls -hAl"
alias less="less -R" # colorise
# Don't alias iptables as this interferes with other iptables functionality
alias iptablesl="sudo iptables --line-numbers -nvL"
# TODO: there's probably some sort of ag config file somewhere, so we shouldn't need an alias here.
# Could also consider changing to rg. Check what incompatibilities there might be. Likely none for
# my usage.
alias ag="ag -W 100 --hidden --path-to-ignore ~/.ignore"
alias sag="sag --hidden --path-to-ignore ~/.ignore"
alias netstat="netstat -plunt"
alias grep="grep --color=auto"
alias terman="$TERM man"
alias dir="dir --color=auto"
alias rsync="rsync -r --progress"
alias strace="strace -v -s 100000"
alias feh="feh -FZ"
alias fehh="feh --info 'echo \"\$(ls \"\$(dirname %F)\" | wc -l) \$(du -s %F | cut -f1) \$(basename %F)\"'"
alias vt="v -t"
alias mountl="mount | column -t"
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
alias vim="nvim"
alias e="emacsclient -t"
alias cp="cp --reflink=auto" # enables instant 'copy' in supporting file systems, e.g. btrfs
alias helm291="~/Downloads/helm-2-9-1/helm"
alias helm216="~/Downloads/helm-2-16-7/helm"

alias -g pg="| egrep"

# Open the following file suffixes in vim
alias -s cpp=vim
alias -s c=vim
alias -s go=vim
alias -s hpp=vim
alias -s hs=vim
alias -s h=vim
alias -s json=vim
alias -s js=vim
alias -s md=vim
alias -s rs=vim
alias -s toml=vim
alias -s ts=vim
alias -s yaml=vim
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
alias 'up'=up_dir

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

DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi;

chpwd() {
  print -l $PWD ${(u)dirstack} > $DIRSTACKFILE
  DIRSTACKSIZE=30
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

stopwatch() {
   date1=`date +%s`; while true; do
       echo -ne "\r$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)";
       sleep 0.1;
   done
}

function countdown(){
    date1=$((`date +%s` + $1));
    while [ "$date1" -ne `date +%s` ]; do
        echo -ne "\r$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)";
        sleep 0.1;
    done
}

# Change to the directory containing a given file
function cdf() {
    if [[ -f "$1" ]]; then
        builtin cd $(dirname "$1")
    else
        builtin cd "$1"
    fi;
}
# globbed cdf
function cdrf() {
    cdf **/$1
}

# Change to the first subdirectory of the working directory listed in alphabetical order
function cd1(){
    \cd $(find $PWD -maxdepth 1 -mindepth 1 -type d | sort | head -n 1)
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

function mkscratch() {
    mkdir -p "/proj/scratch/$1"
    cd "/proj/scratch/$1"
}

# Automatically ls on empty line
auto-ls () {
    if [[ $#BUFFER -eq 0 ]]; then
        echo ""
        exa --all --long --git --time-style long-iso --color=always
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

function fuzzy-widget () {
    DIR_PREVIEW='exa --git-ignore --git --tree --level=2 --color=always {}'
    function skim-select-project-directory () {
        find \
            $HOME/projects/scratch \
            $HOME/projects/github.com/*/ \
            $HOME/projects/gitlab.modusbox.io/*/ \
            $HOME/projects/gitlab.preprod.myanmarpay-pre.io/*/ \
            -maxdepth 1 -mindepth 1 -type d | \
            sk --preview "$DIR_PREVIEW"
    }

    function skim-select-git-branch() {
        git branch -l | sk | sed 's/^ *//g'
    }

    function skim-select-files-from-cwd () {
        fd . --min-depth=$1 --max-depth=$2 | sk --multi | tr '\n' ' '
    }

    # TODO: can/should we use broot instead of skim? (remember to use `br` not `broot` (why?))
    function skim-select-current-directory () {
        fd . --type d | sk --preview "$DIR_PREVIEW"
    }

    function skim-select-process-id () {
        PS_PREVIEW='for f in {loginuid,cgroup,cmdline}; do echo -n "${f}:\t"; cat /proc/{1}/"${f}"; echo; done | cat <(echo "exe:\t$(readlink /proc/{1}/exe)\ntty:\t/dev/$(ps --no-header -o tty {1})") - | column -t'
        ps --no-headers -eo pid,cmd | sed -r 's/^(\s+)([0-9]+) (.*)$/\2\1 \3/g' | sk --preview "$PS_PREVIEW" -m | cut -f1 -d' ' | tr '\n' ' '
    }

    echo -e "\nSelect:\n  (p)roject\n  (c)urrent directory\n  process (i)d\n  (f)iles in working directory\n  git (b)ranch"
    read -sk OPT
    zle reset-prompt
    case $OPT in
        "p")
            zle -U "$(skim-select-project-directory)"
            ;;
        "c")
            zle -U "$(skim-select-current-directory)"
            ;;
        "b")
            zle -U "$(skim-select-git-branch)"
            ;;
        "i")
            zle -U "$(skim-select-process-id)"
            ;;
        "f")
            zle -U "$(skim-select-files-from-cwd 0 1)"
            ;;
    esac
}
zle -N fuzzy-widget
bindkey -M viins '^f' fuzzy-widget

# Bind <C-R> to incremental search like normal
# bindkey -M vicmd '^r' history-incremental-pattern-search-backward
# bindkey -M viins '^r' history-incremental-pattern-search-backward

# https://github.com/aperezdc/zsh-fzy
# zstyle :fzy:history lines '30'
# function histfn {
#     builtin fc -L -l -n -r 1 | grep -v '^\/tmp\/' | awk '!seen[$0]++'
# }
# zstyle :fzy:history command histfn
# bindkey -M viins '^r' fzy-history-widget

bindkey -M vicmd '^r' redo
bindkey -M vicmd 'u' undo
bindkey -M viins "${key[Home]}" beginning-of-line
bindkey -M vicmd "${key[Home]}" beginning-of-line
bindkey -M viins "${key[End]}" end-of-line
bindkey -M vicmd "${key[End]}" end-of-line
bindkey -M viins "${key[Delete]}" delete-char
bindkey -M vicmd "^e" edit-command-line
bindkey -M viins '^e' edit-command-line

function _br () { br }
zle -N _br
bindkey -M viins '^b' _br
bindkey -M vicmd '^b' _br

alias cwd="echo -n $PWD | xclip"

# Duplicate command to window name
case $TERM in
    rxvt*|alacritty|xterm-256color)
        # From: http://stackoverflow.com/questions/20727730/dynamic-window-title-in-urxvt-with-zsh
        # Write some info to terminal title.
        # This is seen when the shell prompts for input.
        function precmd {
            vcs_info
            print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
        }
        # Write command, args, working directory to terminal title.
        # This is seen while the shell waits for a command to complete.
        function preexec {
            printf "\033]0;zsh | %s [%s]\a" "$1" "$PWD"
        }
        ;;
esac

# Password generation. At the time of writing, Alacritty is overwriting the output without the echo
# to add a line break. I think. So it perhaps shouldn't be strictly necessary once that's a solved
# problem.
pw () {
    echo "$(tr -dc '[:print:]' < /dev/urandom | head -c 20)"
}

# control-space to make a normal space
bindkey -M emacs "^;" magic-space "^ " magic-space
bindkey -M viins "^;" magic-space "^ " magic-space

# normal space during searches
bindkey -M isearch " " magic-space ";" magic-space

# kubectl completions
# TODO: This is probably slow. Is it better to package these (with nix) and add them to zshrc?
# rustup recommends this:
#   ZSH:
#       $ rustup completions zsh cargo > ~/.zfunc/_cargo
if [ $commands[kubectl] ]; then source <(kubectl completion zsh); fi
if [ $commands[k3d] ]; then source <(k3d completion zsh); fi
if [ $commands[skaffold] ]; then source <(skaffold completion zsh); fi
if [ $commands[rustup] ]; then source <(rustup completions zsh cargo); fi
# Note: requires:
# autoload -U +X bashcompinit && bashcompinit
# (which is run earlier for `stack` completion)
if [ $commands[aws_completer] ]; then complete -C 'aws_completer' aws; fi

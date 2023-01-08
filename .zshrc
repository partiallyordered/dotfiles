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

# bash autocomplete
autoload -U +X bashcompinit && bashcompinit

autoload edit-command-line
zle -N edit-command-line
autoload -Uz compinit && compinit
autoload -U colors && colors
autoload -Uz vcs_info
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
setopt appendhistory extendedglob nomatch notify autopushd pushdsilent \
    pushdtohome pushdminus pushdignoredups interactivecomments
unsetopt beep
# 10 gives us enough time to use the key chord fd to exit insert mode
export KEYTIMEOUT=10
# End of lines configured by zsh-newuser-install

# Stop ssh autocomplete from taking ages
zstyle ':completion:*' hosts off

alias ls="ls -hAl"
alias less="less -R" # colorise
# Don't alias iptables as this interferes with other iptables functionality
alias iptablesl="sudo iptables --line-numbers -nvL"
# TODO: there's probably some sort of ag config file somewhere, so we shouldn't need an alias here.
# Could also consider changing to rg. Check what incompatibilities there might be. Likely none for
# my usage.
# TODO: move all of these, wherever appropriate, to home.nix
alias ag="ag -W 100 --hidden --path-to-ignore ~/.ignore"
alias sag="sag --hidden --path-to-ignore ~/.ignore"
alias netstat="netstat -plunt"
alias grep="grep --color=auto"
alias dir="dir --color=auto"
alias rsync="rsync -r --progress"
alias strace="strace -v -s 100000"
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
alias sc="systemctl"
alias scs="systemctl status"
alias scrs="systemctl restart"
alias e="emacsclient -t"
alias cp="cp --reflink=auto" # enables instant 'copy' in supporting file systems, e.g. btrfs

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
# "${key[Up]}" is currently not bound because we don't want to override the atuin binding
# [[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
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

# Change to the directory containing a given file
function cdf() {
    if [[ -f "$1" ]]; then
        builtin cd $(dirname "$1")
    else
        builtin cd "$1"
    fi;
}

function mkscratch() {
    mkdir -p "~/projects/scratch/$1"
    cd "~/projects/scratch/$1"
}

# Automatically ls on empty line
# Lifted from: https://stackoverflow.com/a/30183298
my-accept-line () {
    # check if the buffer does not contain any words
    if [ ${#${(z)BUFFER}} -eq 0 ]; then
        # put newline so that the output does not start next
        # to the prompt
        echo
        exa --all --long --git --time-style long-iso --color=always
    fi
    # in any case run the `accept-line' widget
    zle accept-line
}
# create a widget from `my-accept-line' with the same name
zle -N my-accept-line
# rebind Enter, usually this is `^M'
bindkey '^M' my-accept-line

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
# Could potentially just import this file:
# - https://github.com/rwe/oh-my-zsh/blob/7cf783a6a63bc08e00110c6171e0196679bd8337/plugins/vi-mode/vi-mode.plugin.zsh#L18
zle-keymap-select () {
    if [ $KEYMAP = vicmd ]; then
        echo -ne "\033[2 q"
    else
        echo -ne "\033[6 q"
    fi
}
zle -N zle-keymap-select
zle-line-init () { zle -K viins }
zle-line-finish () { zle -K viins }
zle -N zle-line-init
# Set vim insert mode
bindkey -v

# TODO: a lot of this functionality could probably be quite nicely replaced with broot, which also
# comes with much more additional utility
# - the git stuff is pretty well replaced by lazygit
# - the directory/files stuff is served by shell autocorrect and 
# - the only thing I'm really using is the project directory selection which would be better served
#   by broot if it was slightly more responsive when opening larger directories (I *think* it might
#   have a breadth-first directory traversal mode which might solve this?)
# - could also be useful to just call into broot from these functions
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

    echo -e "\nSelect:\n  (p)roject\n  (c)urrent directory\n  process (i)d\n  (f)iles in working directory\n  git (b)ranch\n  files in working (t)ree"
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
        "t")
            zle -U "$(skim-select-files-from-cwd 0 100000)"
            ;;
        # TODO: add history directory option
        # "h") zle -U "$(skim-select-directory-history)"
        # ;;
    esac
}
zle -N fuzzy-widget
bindkey -M viins '^f' fuzzy-widget

function broot-widget () {
    zle -U "$(br | gsar -F '-s:x0A' -'r ' 2>/dev/null)"
}
zle -N broot-widget
bindkey -M viins '^b' broot-widget

# TODO: make j,k in vicmd only go through history in the session, not global/host/whatever history
bindkey -M vicmd '^r' redo
bindkey -M vicmd 'u' undo
bindkey -M viins "${key[Home]}" beginning-of-line
bindkey -M vicmd "${key[Home]}" beginning-of-line
bindkey -M viins "${key[End]}" end-of-line
bindkey -M vicmd "${key[End]}" end-of-line
bindkey -M viins "${key[Delete]}" delete-char
bindkey -M vicmd "^e" edit-command-line
bindkey -M viins '^e' edit-command-line
bindkey -M viins '^l' autosuggest-accept

# control-space to make a normal space
bindkey -M emacs "^;" magic-space "^ " magic-space
bindkey -M viins "^;" magic-space "^ " magic-space

# normal space during searches
bindkey -M isearch " " magic-space ";" magic-space

# TODO: these are probably slowing down shell startup. Is it better to package these (with nix) and
# - add them to zshrc?
# - put them in zsh's fpath a la https://github.com/ahmetb/kubectx/issues/285#issuecomment-1147334645 ?
# rustup recommends this:
#   ZSH:
#       $ rustup completions zsh cargo > ~/.zfunc/_cargo
# We could similarly:
#   $ atuin gen-completions --shell zsh > ~/.zfunc/_atuin
if [ $commands[kubectl] ]; then source <(kubectl completion zsh); fi
if [ $commands[k3d] ]; then source <(k3d completion zsh); fi
if [ $commands[skaffold] ]; then source <(skaffold completion zsh); fi
if [ $commands[rustup] ]; then source <(rustup completions zsh cargo); fi
# Note: each of these requires:
# autoload -U +X bashcompinit && bashcompinit
if [ $commands[aws_completer] ]; then complete -C 'aws_completer' aws; fi
if [ $commands[stack] ]; then eval "$(stack --bash-completion-script stack)"; fi

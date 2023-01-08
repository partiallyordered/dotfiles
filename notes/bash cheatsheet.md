#### Inline temp file ("process substitution")
Note: `bash` or `zsh`, not `sh`
```bash
$ cat <(echo "your face")
your face
$ echo <(echo "your face")
/proc/self/fd/11
$ ls <(echo "your face")
lr-x------ /proc/self/fd/11 -> pipe:[6359673]
```
Creates a temporary file with the output of a command, returns the path as output, allowing the
recipient to read the content of the output as a file. This is called process substitution.

Process substitution will not work with `sudo`, because [it closes file descriptors other than
stdin, stdout, stderr][1]. It's possible to use the `-C` or `--close-from` arguments to `sudo`, but
this requires permission in the `/etc/sudoers` file:
```
Defaults closefrom_override
```

[1]: https://stackoverflow.com/a/39549585

#### Get the directory containing the script being executed
From: https://stackoverflow.com/a/246128:
It will work as long as the last component of the path used to find the script is not a symlink
(directory links are OK). If you also want to resolve any links to the script itself, you need a
multi-line solution:
```sh
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
```

Some of the comments at the above link suggest that the below is better:
```sh
DIR=$(dirname "$(readlink -f "$0")")
```

#### Recursive text find-and-replace
From https://stackoverflow.com/a/1583282
```sh
find /home/www \( -type d -name .git -prune \) -o -type f -print0 | xargs -0 sed -i 's/subdomainA\.example\.com/subdomainB.example.com/g'
```

#### Create a file (_Here Document_)
```sh
cat > file.txt << EOF
Some text
Can contain a number of lines or whatever
Will
be
reproduced  
  verbatim (including those awful trailing spaces above)
  and substitution etc. works too: $(uuidgen)
EOF
```
Documentation: https://tldp.org/LDP/abs/html/here-docs.html
Some relevant info: https://superuser.com/a/1437122

#### Store heredoc in a variable
```sh
INTERPOLATION=this
cat <<- EOF
also works with string $INTERPOLATION
EOF
```

```sh
$ read -r -d '' VAR <<'EOF'
abc'asdf"
$(dont-execute-this)
foo"bar"''
EOF
```
From: https://stackoverflow.com/a/1655389

#### Preserve indentation in a string (using here doc)
```sh
VAR=$(cat << EOF
Some text
    Can contain a number of lines or whatever
Will
be
    reproduced  
verbatim (including those awful trailing spaces above
  and this terrible indentation)
  and substitution etc. works too: $(uuidgen)
EOF
)
```
See also: https://stackoverflow.com/questions/23929235/multi-line-string-with-extra-space-preserved-indentation

#### Trap errors
Does not affect the normal functioning of `set -e`.
```sh
trap 'echo "trapped"' ERR
# or
message() {
    echo "message"
}
trap message ERR
```

#### Wait for condition
```sh
# suppress the output by piping to /dev/null
# until kubectl get pods whatever &> /dev/null
until kubectl get pods nonexistentpod
do
    echo "Waiting for nonexistentpod"
    sleep 1
done
```

#### Loops
From https://stackoverflow.com/questions/8880603/loop-through-an-array-of-strings-in-bash
Much more there.
```sh
declare -a arr=("element1"
                "element2" "element3"
                "element4"
                )
for i in "${arr[@]}"
do
    echo "$i"
done
```
or
```sh
for i in a b c d
# or to preserve spaces
for i in 'a b' 'c d'
# or
for i in a \
         b \
         c \
         d
# or
for i in a b c d
do
    echo "$i"
done
```
```sh
for i in {1..5}
do
    echo "$i"
done
```

#### Handle signals
In this example, we port-forward two services with kubectl then, on a received signal, kill them
both.
```sh
#!/usr/bin/env sh

export KUBECONFIG=~/.kube/config.dev

kubectl port-forward service/nginx 8000:80 &
kubectl port-forward service/redis 6379 &

clean_up() {
    # Note: this works because it attempts to kill every "word" in the output of `jobs -p`. But
    # that means it attempts to kill a lot of process ids that are totally invalid. See the output
    # of `jobs -p` to see all the gumpf it produces.
    # kill $(jobs -p)

    # This is less portable, but much nicer, and kills all subprocesses of the current session
    pkill -s $$
}

trap clean_up SIGHUP SIGINT SIGTERM

sleep infinity
```

#### Import a file of key-value pairs into environment
Given a file `vars.sh` such as
```sh
KEY=value
```
bring these variables into your environment with
```sh
. ./vars.sh
```
or
```sh
source ./vars.sh
```

To export all env vars such that subprocesses started by your shell can use them:
```
set -a
. ./vars.sh
set +a
```
`set -a` and `set +a` can go inside the script `vars.sh`.

Use a `.envrc` file with direnv or similar.

#### Make scripts suck less
Summary: use `set-euxo pipefail`.

##### Exit on errors
```sh
set -e
```
##### Error when an error occurs in a pipeline
```sh
set -o pipefail
```
##### Print all commands as they're about to happen
```sh
set -x
```
##### Fail on missing variables
```sh
set -u
```
##### More:
https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/

##### Shellcheck

https://github.com/koalaman/shellcheck

Integrated in nix: https://github.com/NixOS/nixpkgs/blob/755d7a0735d25192f647b5f85c50d3faf22fccb2/pkgs/build-support/trivial-builders.nix#L253-L274

#### Generate a random UUID
```sh
cat /proc/sys/kernel/random/uuid
```
or
```sh
uuidgen
```

#### Checksum file
First, get the file checksum from somewhere:
```sh
$ sha256sum OpenSSL_1_1_1f.tar.gz
76b78352bc8a9aaccc1de2eb4a52fa9c7f6a5980985242ce3514b0cd208642d3  OpenSSL_1_1_1f.tar.gz
```
Now, use the generated file checksum. *Note* this uses _bash_ process substitution, a feature not
available in plain `sh`:
```bash
sha256sum -c <(echo '76b78352bc8a9aaccc1de2eb4a52fa9c7f6a5980985242ce3514b0cd208642d3  OpenSSL_1_1_1f.tar.gz') --strict
```

#### Conditionals
```sh
if [[ 1 -eq 1 ]]; then
    echo 'duh'
else
    echo 'wtf'
fi
```
```sh
if [ "1" = "1" ]; then
    echo 'duh'
else
    echo 'wtf'
fi
```
On a command result
```sh
if ./some-command > /dev/null; then
    echo "note the /dev/null pipe to suppress the output"
else
    echo "command failed"
fi
```
In a single line
```sh
if [[ "1" = "1" ]]; then echo "okay"; else echo "uh oh"; fi
```
Without an `if` (which doesn't work in zsh)
```sh
[[ 1 == 0 ]] && echo "okay" || echo "fail"
```

#### Pipe stdout and stderr
Reference: https://stackoverflow.com/questions/363223/how-do-i-get-both-stdout-and-stderr-to-go-to-the-terminal-and-a-log-file

Redirect stderr to stdout using `2>&1` then pipe:
```
ls 2>&1 | wc -l
```
(why one would run that example command is another question..)

Shorthand (since bash 4.0):
```bash
ls |& wc -l
```
Pipe only stderr. Order of redirects appears to matter:
```sh
ls 2>&1 1>/dev/null | wc -l
```


##### Pipe stdout and stderr to a file
This says: pipe `ls` to `file.txt` and redirect fd 2 to wherever fd 1 is going.
```sh
ls > file.txt 2>&1
```
Append stdout and stderr to a file:
```sh
ls >> file.txt 2>&1
```
Slightly more simply (since bash 4.0):
```sh
# truncate:
ls &> file.txt
# or, append
ls &>> file.text
```

#### Capture all output from a subshell
Reference: https://stackoverflow.com/questions/363223/how-do-i-get-both-stdout-and-stderr-to-go-to-the-terminal-and-a-log-file
```sh
{
    echo "hello"
    echo "there"
} | tee test
```

#### Read piped data into a variable
```sh
shopt -s lastpipe # run the last pipe command in the parent shell context
echo "hello" | read HELLO
echo $HELLO
```

#### Parse command-line
```sh
while [[ $# > 0 ]]
do
    param_name="$1"
    shift

    case $param_name in
        # a switch, without an argument
        -h|--hold)
            HOLD=true
            ;;
        -c|--color)
            COLOR=true
            ;;
        # an argument
        -n|--name)
            NAME="$1"
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unrecognised parameter"
            usage
            exit 1
            ;;
    esac
done
```

#### Find string in string array
Taken wholesale from: https://stackoverflow.com/a/8574392
```sh
containsElement () {
  local e match="$1"
  shift
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}

# usage
array=("something to search for" "a string" "test2000")
if containsElement "a string" "${array[@]}"; then
    echo "found"
else
    echo "not found"
fi

# "inline array definition" (sorta, hence the quote marks):
if containsElement "a string" "not a string" "something else" "blah blah"; then
    echo "found"
else
    echo "not found"
fi
```

#### Generate files containing fake data
```bash
#!/usr/bin/env bash
NUM_FILES=3
LINES_PER_FILE=10
IFS=$'\n'
for i in $(fakedata --limit "$NUM_FILES" name)
do
    fakedata --limit "$LINES_PER_FILE" email country occupation > "$i"
done
unset IFS
```

#### termdown
>   Hotkeys:
>
>       E       Show end time (countdown mode only)
>       L       Lap (stopwatch mode only)
>       R       Reset
>       SPACE   Pause (will delay absolute TIME)
>       +       Plus (will add 10 seconds)
>       -       Minus (will subtract 10 seconds)
>       Q       Quit

##### Countdown
>   Starts a countdown to TIME.
>
>   Example values for TIME: 10, '1h 5m 30s', '12:00', '2020-01-01', '2020-01-01 14:00 UTC', '5pm'
```sh
nix-shell -p termdown --run 'termdown 5m'
# or e.g.
nix-shell -p termdown --run 'termdown 5 && notify-send "five seconds elapsed!"'
```

##### Stopwatch
```sh
nix-shell -p termdown --run termdown
```

#### Notify completed task
The echo rings the bell, which terminals will typically use as a prompt to make the window more
interesting to the user.
```sh
sleep 5; notify-send "done"; printf '\a'
```

#### Command-line parameter default values
```sh
echo ${1:-"this is the default value"}
DEFAULT="default"
echo ${1:-$DEFAULT}
```

#### Quick code measurement
```sh
nix-shell -p scc --command scc
```
or
```sh
nix-shell -p tokei --command tokei
```

#### Info about a command
```sh
which grep
```
Do a path search for grep even if it is an alias, reserved word, shell function or builtin:
```sh
which -p grep
```

#### Regex string replace
```sh
VAR="hello, there"
echo ${VAR/t?ere/world}
```

#### Check if shell is interactive
Source: https://www.gnu.org/software/bash/manual/html_node/Is-this-Shell-Interactive_003f.html

To determine within a startup script whether or not Bash is running interactively, test the value
of the `-` special parameter. It contains i when the shell is interactive. For example:
```bash
case "$-" in
*i*)	echo This shell is interactive ;;
*)	echo This shell is not interactive ;;
esac
```

Alternatively, startup scripts may examine the variable PS1; it is unset in non-interactive shells,
and set in interactive shells. Thus:
```bash
if [ -z "$PS1" ]; then
  echo This shell is not interactive
else
  echo This shell is interactive
fi
```

#### Check if script is being executed in pipe

In a pure POSIX shell
```sh
if [ -t 1 ] ; then echo terminal; else echo "not a terminal"; fi
```
returns "terminal", because the output is sent to your terminal, whereas
```sh
(if [ -t 1 ] ; then echo terminal; else echo "not a terminal"; fi) | cat
```

returns "not a terminal", because the output of the parenthetic element is piped to cat.

Ref: https://stackoverflow.com/questions/911168/how-can-i-detect-if-my-shell-script-is-running-through-a-pipe

#### Multi-line string replace
Use gsar:
```sh
echo "hello\nworld" | gsar -F '-s:x0A' '-r' 2>/dev/null
```
One problem with this approach: gsar is not widely available. Consider a scripting language.

Also possible with ripgrep:
```sh
echo 'apple\norange\nbanana\nkiwifruit' | rg --passthru -U '(?s)orange.*kiwi' -r jack
```

Ref: https://learnbyexample.github.io/substitution-with-ripgrep/

##### In-place in a file
Use one of the previous approaches for replacement, combined with _sponge_:
```sh
echo 'apple\norange\nbanana\nkiwifruit' > infile
rg --passthru -U '(?s)orange.*kiwi' -r jack infile | sponge infile
```
Or:
```sh
nix-shell -p sd --command "sd 'orange\nbanana\nkiwi' jack infile"
```
Or, theoretically, it's possible to use `rpl`. Of particular interest is `rpl -p`, to use `rpl` in
interactive mode. However, at the time of writing, this is broken in nixpkgs, like every other
piece of Python software.
```sh
nix-shell -p rpl
```
Instead, consider `up` for "interactive" mode:
```sh
nix-shell -p up sd
up < infile
# now type the following in the up shell:
sd -p 'orange\nbanana\nkiwi' jack
# now press ctrl+x to save the script you typed
```

#### Natively replace substring in bash variable value
```sh
$ echo "$PWD"
/home/user/projects/github.com/org/repo/src/test/java/com/org/project/web/rest
# change to the corresponding source directory by replacing "test" with "main":
$ cd ${$PWD/test/main}
```

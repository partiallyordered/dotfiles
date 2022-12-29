
# git difftool <revision_1> <revision_2> ./path/to/file
# e.g.
# back two commits
git difftool HEAD HEAD^^ ./main.c
# back five commits
git difftool HEAD HEAD~5 ./main.c
# specific commit hash
git difftool HEAD 3e15a9967bc9322a684d3453739c9331d8409793 ./main.c
# one before a specific commit hash (note the escaped caret, \^)
git difftool HEAD 3e15a9967bc9322a684d3453739c9331d8409793\^ ./main.c

# or

git difftool <revision_1>:<./path/to/file_1> <revision_2>:<./path/to/file_2>
git difftool HEAD:./old_file_name.c 3e15a9967bc9322a684d3453739c9331d8409793:./new_file_name.c

# difftool against another branch
git difftool <other-branch>
# or (will open temporary files instead of the file at HEAD)
git difftool <other-branch> HEAD

# between branches
git difftool somebranch otherbranch -- main.c

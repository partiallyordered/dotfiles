
#### Search and replace in visual block selection
```
:s/\%Vsearch/replace/
```
Works with other switches like `\v`:
```
:s/\%V\vsearch/replace/
```

#### Inverse :g selection
e.g.:
`:v/term/:norm do thing`
Mnemonic: perhaps related to `grep -v`.

#### Modify a character in a macro, where the macro is bound to q
This sequence of keys will paste the macro contents into the command buffer
```vimscript
:let @q='<c-r>q
```
#### Now edit as necessary, close the single-quote and press enter

#### C/js modeline example
```
// vim: ts=4:sw=4:expandtab
```

#### Split current buffer
Split or vsplit:
`:sp`/`<Ctrl+w>s` or `:vsp`/`<Ctrl+w>v`
To close:
`<Ctrl+w>c`
Close all except the currently focused:
`<Ctrl+w>o`
Move around:
`<Ctrl+w>j`
`<Ctrl+w>h`
`<Ctrl+w>k`
`<Ctrl+w>l`
`<Ctrl+w>w`
Resize: probably just use cursor
`<Ctrl+w>+`
`<Ctrl+w>-`
Equalise:
`<Ctrl+w>=`
Close preview windows:
`:pc` or `<Ctrl+w>z`

#### Set indent depth of block
From: https://vim.fandom.com/wiki/Shifting_blocks_visually
1. Make visual selection
2. Type `:le 12` for 12-space indent
To eliminate the indent, just use `:le`.

#### Don't break lines
```vimscript
set tw=0 wm=0
```

#### Configure LanguageClient-neovim
Place settings in a `.vim/settings.json` file at the project root.

For rust-analyzer configuration options see:
https://rust-analyzer.github.io/manual.html#configuration. An example file demonstrating a couple
of items of interest (redundantly):

```json
{
    "rust-analyzer": {
        "cargo": {
            "features": [
                "reqwest"
            ],
            "allFeatures": true
        }
    }
}
```

#### Change case with regex
From: https://vim.fandom.com/wiki/Changing_case_with_regular_expressions

In a substitute command, place `\U` or `\L` before backreferences for the desired output.
Everything after `\U`, stopping at `\E` or `\e`, is converted to uppercase. Similarly, everything
after `\L`, stopping at `\E` or `\e`, is converted to lowercase.

Alternatively, use `\u` to uppercase only the first character of what follows, or `\l` to lowercase
only the first character.

For example, assume a line with the text "This is a test".
```
:s/\(test\)/\U\1 file/
```
produces: This is a TEST FILE
```
:s/\(test\)/\U\1\e file/
```
produces: This is a TEST file

#### Open remote file over ssh / scp
Note
- the absence of a colon before the path (the forward slash is a separator)
- the path example here is absolute, but needn't be
```sh
vim scp://user@myserver[:port]/[/path/to/file.txt]
# e.g.
vim scp://user@myserver//home/user/file.txt
```
Slightly more generally:
```sh
vim protocol://[user@]hostname[:port]/[path]
```

#### Disable wrapping
```
:set tw=0 wm=0 nowrap
```
https://stackoverflow.com/questions/1290285/why-cant-i-stop-vim-from-wrapping-my-code

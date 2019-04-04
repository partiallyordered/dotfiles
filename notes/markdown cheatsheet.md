
### Links within document

From https://stackoverflow.com/a/16426829

Github automatically parses anchor tags out of your headers. So you can do the following:

```no-highlight
[Custom foo description](#foo)
# Foo
```

In the above case, the Foo header has generated an anchor tag with the name foo

Note: just one # for all heading sizes, no space between # and anchor name, anchor tag names must be lowercase, and delimited by dashes if multi-word.

```no-highlight
[click on this link](#my-multi-word-header)
### My Multi Word Header
```

### Links within repo
```no-highlight
[Some other doc](./some_other_doc.md)
[Another doc with spaces in the filename](./another%20doc%20with%20spaces%20in%20the%20filename)
```

### More markdown tips

https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet

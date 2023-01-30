
### Tables
From: https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#tables

Colons can be used to align columns.

| Tables        | Are           | Cool  |
| ------------- |:-------------:| -----:|
| col 3 is      | right-aligned | $1600 |
| col 2 is      | centered      |   $12 |
| zebra stripes | are neat      |    $1 |

There must be at least 3 dashes separating each header cell.
The outer pipes (|) are optional, and you don't need to make the
raw Markdown line up prettily. You can also use inline Markdown.

Markdown | Less | Pretty
--- | --- | ---
*Still* | `renders` | **nicely**
1 | 2 | 3

Note, when using vim-easy-align, select a table then use `<leader>ga*|` to align all table columns
in the source.

### Links within document

From https://stackoverflow.com/a/16426829

Github automatically parses anchor tags out of your headers. So you can do the following:

```no-highlight
[Custom foo description](#foo)
# Foo
```

In the above case, the Foo header has generated an anchor tag with the name foo

Note: just one # for all heading sizes, no space between # and anchor name, anchor tag names must
be lowercase, and delimited by dashes if multi-word.

```no-highlight
[click on this link](#my-multi-word-header)
### My Multi Word Header
```

#### Link target overloading

```md
### Section title
[link to first section title]("section-title")
[link to second section title]("section-title-1")

### Section title <a id='section-title-1'></a>
```

### Links within repo
```no-highlight
[Some other doc](./some_other_doc.md)
[Another doc with spaces in the filename](./another%20doc%20with%20spaces%20in%20the%20filename)
```

### External links
[Same as internal links, basically](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#links)

### Images
Lifted directly from https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#images

```md
Here's our logo (hover to see the title text):

Inline-style:
![alt text](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 1")

Reference-style:
![alt text][logo]

[logo]: https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 2"
```

### References in markdown

From: https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#links

[I'm a reference-style link][Arbitrary case-insensitive reference text]

[You can use numbers for reference-style link definitions][1]

Or leave it empty and use the [link text itself].

[arbitrary case-insensitive reference text]: https://www.mozilla.org
[1]: http://slashdot.org
[link text itself]: http://www.reddit.com

### References

https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet

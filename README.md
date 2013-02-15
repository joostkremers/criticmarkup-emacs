`cm-mode` is a minor mode that provides (rudimentary) support for
[CriticMarkup](http://criticmarkup.com/) in Emacs.

CriticMarkup defines the following patterns for marking changes to a text:

- Addition {++ ++}
- Deletion {-- --}
- Substitution {~~ ~> ~~}
- Comment {>> <<}
- Highlight {{ }}{>> <<}

Note: additions are called insertions here, because it allows us to use mnemonic key bindings.

Activating `cm-mode` provides key bindings to insert the patterns above and thus mark one's changes to the text. The provided key bindings are:

- `C-c * i`: insert text
- `C-c * d`: delete text
- `C-c * s`: substitute text
- `C-c * c`: insert a comment
- `C-c * h`: highlight text and insert a comment

The commands to delete, substitute and highlight text all operate on the region. The commands for inserting and substituting text and for inserting a comment (which includes the command to highlight text) all put the cursor at the correct position, so you can start typing right away.

`cm-mode` also adds the markup patterns defined by CriticMarkup to `font-lock-keywords` and provides customisable faces to highlight them. The customisation group is called `criticmarkup`.


TODO:

- Commands to accept or reject the change at point (`C-c * a` and `C-c * r`)
- Command to accept or reject all changes interactively (`C-c * A`)
- Mouse support

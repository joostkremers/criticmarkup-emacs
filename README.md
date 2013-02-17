# CriticMarkup for Emacs #

`cm-mode` is a minor mode that provides (rudimentary) support for
[CriticMarkup](http://criticmarkup.com/) in Emacs.

CriticMarkup defines the following patterns for marking changes to a text:

- Addition {++ ++}
- Deletion {-- --}
- Substitution {~~ ~> ~~}
- Comment {>> <<}
- Highlight {{ }}{>> <<}

Note: additions are called insertions here, because it allows us to use mnemonic key bindings. {>>That's because 'a' is reserved for accepting a change. Perhaps you should mention that?<<}

Activating `cm-mode` provides key{--s--} {++bindings++}to insert the {~~patterns~>markup~~} above and thus mark one's changes to the text. The provided key bindings are:

- `C-c * i`: insert text
- `C-c * d`: delete text
- `C-c * s`: substitute text
- `C-c * c`: insert a comment
- `C-c * h`: highlight text and insert a comment

The commands to delete, substitute and highlight text all operate on the region. The commands for inserting and substituting text and for inserting a comment {{(which includes the command to highlight text)}}{>>really!<<} all put the cursor at the correct position, so you can start typing right away.


## Font lock ##

`cm-mode` also adds the markup patterns defined by CriticMarkup to `font-lock-keywords` and provides customisable faces to highlight them. The customisation group is called `criticmarkup`.

You may notice that changes that span multiple lines are not highlighted. The reason for this is that multiline font lock in Emacs is not straightforward. There are ways to deal with this, but since `cm-mode` is a minor mode, it could interfere with the major mode's font locking mechanism if it did that.

To mitigate this problem, you can use soft wrap (with `visual-line-mode`). Since each paragraph is then essentially a single line, font lock works even across multiple (visual) lines.


## TODO ##

- Command to accept or reject all changes interactively (`C-c * A`)
- Mouse support

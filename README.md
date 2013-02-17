# CriticMarkup for Emacs #

`cm-mode` is a minor mode that provides (rudimentary) support for
[CriticMarkup](http://criticmarkup.com/) in Emacs.

CriticMarkup defines the following patterns for marking changes to a text:

- Addition {++ ++}
- Deletion {-- --}
- Substitution {~~ ~> ~~}
- Comment {>> <<}
- Highlight {{ }}{>> <<}

Activating `cm-mode` provides key{--s--} {++bindings ++}to insert the {~~patterns~>markup~~} above and thus mark one's changes to the text. The provided key bindings are: {>>Should you mention that these are nicely mnemonic?<<}

- `C-c * a`: add text
- `C-c * d`: delete text
- `C-c * s`: substitute text
- `C-c * c`: insert a comment
- `C-c * h`: highlight text and insert a comment

The commands to delete, substitute and highlight text all operate on the region. The commands for inserting and substituting text and for inserting a comment {{(which includes the command to highlight text)}}{>>That's good<<} all put the cursor at the correct position, so you can start typing right away.


## Accepting or rejecting changes ##

You can interactively accept or reject a change by putting the cursor inside it and hitting `C-c * i`. For additions, deletions and substitutions, you get a choice between `a` to accept the change or `r` to reject it. There are two other choices, `s` to skip this change or `q` to quit. Both leave the change untouched and if you're just dealing with the change at point, they are essentially identical. (They have different functions when accepting or rejecting all changes interactively, though.)

For comments and highlights, the choices are different: `d` to delete the comment or highlight (whereby the latter of course retains the highlighted text, but the comment and the markup are removed), or `k` to keep the comment or highlight. Again `q` quits and is essentially identical to `k`. (Note that you can also use `s` instead of `k`, in case you get used to skipping changes that way.)

Not implemented yet is the ability to go through all changes in a buffer and accept or reject them one by one, nor the ability to accept or reject all changes all at once.


## Font lock ##

`cm-mode` also adds the markup patterns defined by CriticMarkup to `font-lock-keywords` and provides customisable faces to highlight them. The customisation group is called `criticmarkup`.

You may notice that changes that span multiple lines are not highlighted. The reason for this is that multiline font lock in Emacs is not straightforward. There are ways to deal with this, but since `cm-mode` is a minor mode, it could interfere with the major mode's font locking mechanism if it did that.

To mitigate this problem, you can use soft wrap (with `visual-line-mode`). Since each paragraph is then essentially a single line, font lock works even across multiple (visual) lines.


## TODO ##

- Command to accept or reject all changes interactively (`C-c * I`)
- Commands to accept or reject all changes in one go
- Mouse support

# CriticMarkup for Emacs #

`cm-mode` is a minor mode that provides support for [CriticMarkup](http://criticmarkup.com/) in Emacs.

CriticMarkup defines the following patterns for marking changes to a text:

- Addition {++ ++}
- Deletion {-- --}
- Substitution {~~ ~> ~~}
- Comment {>> <<}
- Highlight {{ }}{>> <<}

Activating `cm-mode` provides key{--s--} {++bindings ++}to insert the {~~patterns~>markup~~} above and thus mark one's changes to the text. The provided {{key bindings}}{>>Should you mention that these are nicely mnemonic?<<} are:

- `C-c * a`: add text
- `C-c * d`: delete text
- `C-c * s`: substitute text
- `C-c * c`: insert a comment (possibly with highlight)

The commands to delete or substitute text operate on the region. The command to insert a comment can be used with an active region, in which case the text in the region will be highlighted. It can also be used inside an existing markup to add a comment to it. If it is used anywhere else, it just adds a lone comment. The commands for inserting and substituting text and for inserting a comment all put the cursor at the correct position, so you can start typing right away. 


## Follow changes mode ##

`cm-mode` also provides a simple 'follow changes' mode. When activated, changes you make to the buffer are automatically marked as insertions or deletions. Substitutions cannot be made automatically (that is, if you mark a word, delete it and then type a replacement, it will still be marked as sequence of deletion+insertion, not as a substitution), but they can still be made manually with `C-c * s`. You can activate and deactivate follow changes mode with `C-c * F`. When it's active, the modeline indicator for `cm-mode` changes from `cm` to `cm*`. 

Follow changes mode should be considered experimental, so try at your own risk. If you run into problems, open an issue on Github or send me an email.


## Accepting or rejecting changes ##

One can interactively accept or reject a change by putting the cursor inside it and hitting `C-c * i`. For additions, deletions and substitutions, you get a choice between `a` to accept the change or `r` to reject it. There are two other choices, `s` to skip this change or `q` to quit. Both leave the change untouched and if you're just dealing with the change at point, they are essentially identical. {>>They have different functions when accepting or rejecting all changes interactively, though.<<}

For comments and highlights, the choices are different: `d` to delete the comment or highlight (whereby the latter of course retains the highlighted text, but the comment and the markup are removed), or `k` to keep the comment or highlight. Again `q` quits and is essentially identical to `k`. (Note that you can also use `s` instead of `k`, in case you get used to skipping changes that way.)

You can interactively accept or reject all changes with `C-c * I` (that is a capital `i`). This will go through each change asking you whether you want to accept, reject or skip it, or delete or keep it. Typing `q` quits the accept/reject session.


## Font lock ##

`cm-mode` also adds the markup patterns defined by CriticMarkup to `font-lock-keywords` and provides customisable faces to highlight them. The customisation group is called `criticmarkup`.

You may notice that changes that span multiple lines are not highlighted. The reason for this is that multiline font lock in Emacs is not straightforward. There are ways to deal with this, but since `cm-mode` is a minor mode, it could interfere with the major mode's font locking mechanism if it did that.

To mitigate this problem, you can use soft wrap (with `visual-line-mode`). Since each paragraph is then essentially a single line, font lock works even across multiple (visual) lines.


## TODO ##

- Commands to accept or reject all changes in one go. {>>These won't be bound to keys, though.<<}
- Mouse support?

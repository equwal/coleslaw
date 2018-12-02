COLESLAW-MODE
==========

## A Major Mode For [Coleslaw][Coleslaw]
	[Coleslaw]: https://github.com/kingcons/coleslaw
---
(Notice: very early prototype. Very few features.)
## Features:
* `M-;` inserts the comment block at the top of the page like this:

``` common-lisp
;;;;;

;;;;;
```

## Install:
1. Clone in your `~/.emacs.d/` or equivalent staging area.
2. Add this line to your init file (maybe at `~/.emacs.d/init.el`). This assumes that you cloned into `~/.emacs.d/`, change it to fit.

``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/coleslaw-mode/")

```
3. For markdown syntax highlighting, `markdown-mode` must be installed from MELPA.
4. For a preview of the markdown, `markdown-preview-mode` must be installed, along with a bash shell 
command, `markdown`. 
5. Open a `.page` or `.post` file in emacs!

## TODO:

* Insert automatically `format: `, `title: `, and `url: ` with `M-;`.
* Only use markdown-mode if `format: md` is used.
* Setup a lispy mode if `format: cl-who` is used.
* Setup a **LaTeX** mode if..., etc.
* Bind emacs commands for interacting with coleslaw

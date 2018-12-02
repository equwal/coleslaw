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
3. **Install dependencies from MELPA**
   * For markdown syntax highlighting, [`markdown-mode`][markdown-mode].
For a preview of the markdown, [`markdown-preview-eww`][eww], and the web browser it is setup to use, [w3m][w3m]
	[eww]: https://github.com/niku/markdown-preview-eww
	[w3m]: https://www.emacswiki.org/emacs/emacs-w3m
	[markdown-mode]: https://jblevins.org/projects/markdown-mode/
4. Open a `.page` or `.post` file in emacs!

## TODO:

* Insert automatically `format: `, `title: `, and `url: ` with `M-;`.
* Only use markdown-mode if `format: md` is used.
* Setup a lispy mode if `format: cl-who` is used.
* Setup a **LaTeX** mode if..., etc.
* Bind emacs commands for interacting with coleslaw

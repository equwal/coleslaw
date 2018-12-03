COLESLAW-MODE
==========

## An Emacs Minor Mode For [Coleslaw][Coleslaw]
---
(Notice: very early prototype. Very few features.)
## Features
* `.page` and `.post` files automatically have [Fly Spell][Flyspell] spell checking and [Markdown major mode][markdown-mode] enabled.
* `coleslaw-insert-header` inserts the comment block at the top of the page like this:

```
;;;;;
title: 
url: 
format: 
date: 
;;;;;
```
* `markdown-preview-eww` might be used to view realtime markup.
## Install
1. Clone in your `~/.emacs.d/` or equivalent staging area.
* Add this line to your init file (maybe at `~/.emacs.d/init.el`). This assumes that you cloned into `~/.emacs.d/`; change it to fit.

	```

	(add-to-list 'load-path "~/.emacs.d/coleslaw-mode/")

	```

* ## Install optional dependencies

* For markdown syntax highlighting, [markdown-mode][markdown-mode]. 
* For a preview of the markdown, [markdown-preview-eww][eww], and the web browser it is setup to use, [w3m][w3m]
* Note that [eww][eww] has special OS level dependencies.
4. Open a `.page` or `.post` file in emacs!

## TODO

* Insert automatically `format:`, `title:`, and `url:` with `M-;`.
* Only use markdown-mode if `format: md` is used.
* Setup a lispy mode if `format: cl-who` is used.
* Setup a **LaTeX** mode if..., etc.
* Bind emacs commands for interacting with coleslaw via [slime][slime].

[slime]: https://common-lisp.net/project/slime/
[Flyspell]: https://www.emacswiki.org/emacs/FlySpell
[Coleslaw]: https://github.com/kingcons/coleslaw
[eww]: https://github.com/niku/markdown-preview-eww
[w3m]: https://www.emacswiki.org/emacs/emacs-w3m
[markdown-mode]: https://jblevins.org/projects/markdown-mode/

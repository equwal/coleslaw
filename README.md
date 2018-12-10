COLESLAW-MODE
==========

## An Emacs Minor Mode For [Coleslaw][Coleslaw]


## Features
* `coleslaw-insert-header` (bound to `M-;`) inserts the comment block
  *depending on the type of file*, and *is how the major mode is selected
  for the file*.
* A `*.post` file would insert:
```
;;;;;
title:
format:
date:
;;;;;
<!--more-->

<!--more-->

```

* `.page` and `.post` files automatically enable coleslaw-mode in emacs.
* [Fly Spell][Flyspell] spell checking and [Markdown major mode][markdown-mode] enabled.

* [Markdown-preview-eww][eww] automatically opens a markdown preview on loadup of a coleslaw-mode file.
## Install
1. Clone in your `~/.emacs.d/` or equivalent staging area.
* Add this line to your init file (maybe at `~/.emacs.d/init.el`). This assumes that you cloned into `~/.emacs.d/`; change it to fit.

	```

	(add-to-list 'load-path "~/.emacs.d/coleslaw-mode/")

	```

* ## Install optional dependencies

* For markdown syntax highlighting, [markdown-mode][markdown-mode]. 
* For a preview of the markdown, [markdown-preview-eww][eww].
* Note that [eww][eww] has special OS level dependencies.
* Open a `.page` or `.post` file in emacs!

## TODO

* Bind emacs commands for interacting with coleslaw via [slime][slime].

[slime]: https://common-lisp.net/project/slime/
[Flyspell]: https://www.emacswiki.org/emacs/FlySpell
[Coleslaw]: https://github.com/kingcons/coleslaw
[eww]: https://github.com/niku/markdown-preview-eww
[markdown-mode]: https://jblevins.org/projects/markdown-mode/

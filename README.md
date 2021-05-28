# spaceship-mode
spaceship-mode and tabble-mode: text/code alignment for variable-width fonts

## Purpose

Two minor modes for Emacs, `spaceship-mode` and `tabble-mode`, automatically alter the width of certain spaces and/or tabs in the buffer to align buffer text.  The two modes operate independently and can be used separately or together.  They serve two purposes, broadly speaking:

1. With a variable-width font, these modes provide some of the alignment functionality that you would otherwise have gotten for free by counting characters in a fixed-width font.  This is the main reason I wrote them.
	* Using `spaceship-mode`, the initial spaces on a line will have their width adjusted to match the same number of characters on previous lines, so that the left edge of the text/code will be aligned as it would be in a fixed-width font.
	* Using `tabble-mode`, you can use tabs to align text in other places than the left edge.  `tabble-mode` is essentially an implementation of [elastic tabstops](https://nickgravgaard.com/elastic-tabstops/).
	* Using `tabble-mode` you can quickly create automatically-aligned tables (“tabbles”) in text or code using tab characters as the column separator.
	* Using `spaceship-mode` with `spaceship-auto-preserve` (considered experimental and disabled by default), aligned code blocks will have their alignment preserved in certain situations when the code they are aligned to changes position during editing.

## Caveats

* **Non-standard space/tab conventions.** You must use spaces and tabs consistently in a way that supports the mode.  In particular, with `spaceship-mode` you must strictly adhere to the convention of using tabs for indentation and spaces for alignment; otherwise it won't work.
* **No user interface.** This code is provided as a backend only, and relies on conventions that are contradictory to the assumptions of stock Emacs. It will take some effort to integrate it into your workflow.
* **No promises.** It works on my system, and I'm offering it in hope it will be useful to others.  It uses the `after-change-functions` hook in a heavy way and may be incompatible with other major modes or popular packages.

## What It Does, Precisely

### spaceship-mode

Certain spaces have their widths adjusted to match that of another character on the previous line; generally speaking the nth initial space will match the nth character on the previous line, which might itself be an initial space that matches something further back.  To be precise, a character matches another character exactly when:

* All previous characters on the same line match something, and
* There is a corresponding character on the previous line and it is eligible to be matched by this character:
	* A tab can match only another tab.
	* A space can match any character *except* a tab.
	* Other characters do not match anything.

Each matching space has its width modified to be the same as the character it matches.  This applies recursively: if the matched character's width is itself modified, the modified width is matched.

The rules produce good results when tabs are used for indentation, meaning a standard horizontal offset, and spaces are used for alignment.  The rules allow relative indentation: if a line has spaces and then tabs, the tabs cause indentation by a standard amount relative to the alignment point indicated by the spaces.

All tabs which are part of the leading space of a line (the spaces and tabs occuring before any printing character) have their width standardized by `spaceship-mode`.  The variable `spaceship-tab-pixel-width` determines the width of these tabs.  The usual Emacs variables controlling the display of tabs are ignored (but they still apply for other tabs, unless `tabble-mode` is also used).

### tabble-mode

Let's call tabs which are not part of the leading space *tabble tabs*.  A maximal sequence of consecutive lines containing tabble tabs form a *tabble*.  `tabble-mode` adjusts the width of all tabble tabs so that the columns of each tabble are left-aligned.  Note that the first cell of each row cannot be empty since otherwise there would be no tabble tabs in that line and the tabble would be ended.

### spaceship-auto-preserve

If the variable `spaceship-auto-preserve` is set to `t`, then `spaceship-mode` will also try to modify the leading space of aligned blocks of code to preserve the alignment whenever the alignment point shifts position due to editing.  This feature is considered experimental; see the code for details.

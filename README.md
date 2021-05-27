# spaceship-mode
spaceship-mode and tabble-mode: text/code alignment for variable-width fonts

## Purpose

This repo provides two different minor modes for emacs, `spaceship-mode` and `tabble-mode`, which automatically alter the width of certain spaces and/or tabs in the buffer to align buffer text.  The two modes operate independently and can be used separately or together.  Broadly speaking, there are two purposes served by this:

1. If you want to use a variable-width font, you can use these modes to duplicate some of the alignment functionality that you would otherwise have gotten for free by counting characters in a fixed-width font.  This is the main purpose for which these tools were written.
	* Using `spaceship-mode`, the initial spaces on a line will have their width adjusted to match the same number of characters on previous lines, so that the left edge of the text/code will be aligned as it would be in a fixed-width font.
	* Using `tabble-mode`, you can use tabs in a non-standard way (a.k.a. “elastic tabs”) to align text/code at positions other than the left edge.
3. Certain features may be useful even with fixed-width fonts.
	* Using `tabble-mode` you can quickly create automatically-aligned tables (“tabbles”) in text or code using tab characters as the column separator.
	* Using `spaceship-mode` with `spaceship-auto-preserve` (considered experimental and disabled by default), aligned code blocks will have their alignment preserved in certain situations even when the code they are aligned to changes position.

## Caveats

* **Non-standard space/tab conventions.** You must use spaces and tabs consistently in a way that supports the mode.  In particular, with `spaceship-mode` you must strictly adhere to the convention of using tabs for indentation and spaces for alignment; otherwise it won't work.
* **No user interface.** This code is provided as a backend only, and relies on conventions that are contradictory to the assumptions of stock Emacs. It will take some effort to integrate it into your workflow.
* **No promises.** It works on my system, and I'm offering it in hope it will be useful to others.  It uses the `after-change-functions` hook in a heavy way and may be incompatible with other popular packages.

## How it Works

Markdown preview mode
===========================


[![MELPA Stable](http://stable.melpa.org/packages/markdown-preview-mode-badge.svg)](http://stable.melpa.org/#/markdown-preview-mode)
[![MELPA](http://melpa.org/packages/markdown-preview-mode-badge.svg)](http://melpa.org/#/markdown-preview-mode)

## Description
Opens a preview in a browser, updated upon buffer save.

Same preview window is valid across multiple markdown buffers.
Scrolls browser window to keep your editing position visible.

## Usage

`M-x markdown-preview-mode` will open preview in a browser and will start `markdown-mode` if it's not yet running for current buffer. If you've closed the preview window, you can start it over with `M-x markdown-preview-open-browser`. All websockets will be cleaned up on emacs termination. If you'd like to perform cleanup manually run `M-x markdown-preview-cleanup`.

## Multimarkdown

In order to enable multimarkdown support, customize
`M-x customize-option` -> `markdown-command` variable.
Look for `Markdown Command` which is set to `markdown` by default,
set to `multimarkdown` and make sure it's in your PATH.

## Browser

In order to select preferred browser, customize
`M-x customize-option` -> `browse-url-browser-function` option.
Select your browser from `Value menu`. If it's not there, follow [EmacsWiki: Browse Url](http://www.emacswiki.org/emacs/BrowseUrl).

## Stylesheets

Default theme for preview is [Solarized-dark](http://thomasf.github.io/solarized-css/)

Extra css are added with:

```lisp
(add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
```
Override theme completely with:

```lisp
(setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
```

## Extra javascript

If you'd like to include additional javascript for the preview, add this to your init:

```lisp
(add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
```
or, to make it async:

```lisp
(add-to-list 'markdown-preview-javascript '("http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML" . async))
```

## Websocket port

Adjustable by `M-x customize-option` -> `markdown-preview-port`.

## Installation
### Melpa

*Melpa* recipe is available, so `markdown-preview-mode` is just a `package-install` away!

### el-get
* `M-x el-get-self-update`
* `M-x el-get-install` -> `markdown-preview-mode`


## Dependencies

* [markdown-mode.el](https://github.com/defunkt/markdown-mode)
* [websocket.el](https://github.com/ahyatt/emacs-websocket)

Makes use of `markdown-mode`, which already can transform markdown into html
and `websocket.el` to deliver html to browser.

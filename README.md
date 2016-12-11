Markdown preview mode
===========================

[![MELPA Stable](http://stable.melpa.org/packages/markdown-preview-mode-badge.svg)](http://stable.melpa.org/#/markdown-preview-mode)
[![MELPA](http://melpa.org/packages/markdown-preview-mode-badge.svg)](http://melpa.org/#/markdown-preview-mode)

Markdown preview in emacs features:

* on save/idle preview update
* scroll sync
* custom/extra css and javascript
* remote preview
* multiple simultaneous previews

## Install

* `package-install markdown-preview-mode`
* `el-get-install markdown-preview-mode`

## Run

* `markdown-preview-mode` - start mode and open preview window.
* `markdown-preview-open-browser` - open priview window for current buffer.
* `markdown-preview-cleanup` - cleanup running processes (close websocket and http servers).

## Customize

* `customize-option markdown-command` - change markdown processor.
* `customize-option` [browse-url-browser-function](http://www.emacswiki.org/emacs/BrowseUrl) - select different browser.
* `customize-option markdown-preview-host` - change http/websocket server address.
* `customize-option markdown-preview-ws-port` - change websocket server port.
* `customize-option markdown-preview-http-port` - change http server port.

## Extra css

### Add extra css to default solarized dark theme
```lisp
(add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
```
### Override theme completely with

```lisp
(setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))
```

## Extra javascript

### Add MathJax

```lisp
(add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
```
### async

```lisp
(add-to-list 'markdown-preview-javascript '("http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML" . async))
```

## Dependencies

* [markdown-mode.el](https://github.com/defunkt/markdown-mode)
* [websocket.el](https://github.com/ahyatt/emacs-websocket)
* [web-server.el](https://github.com/eschulte/emacs-web-server)

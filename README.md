Markdown preview mode
===========================

## Description
Opens a preview in a browser, updated upon buffer save.
Same preview window is valid across multiple markdown buffers.
Scrolls browser window to keep your editing position visible.

## Dependencies

* markdown-mode.el
* websocket.el

Makes use of `markdown-mode`, which already can transform markdown into html
and `websocket.el` to deliver html to browser.

## Usage

`M-x markdown-preview-mode` will open preview in a browser and will start `markdown-mode` if it's not yet running for current buffer. If you'v closed the preview window, you can start it over with `M-x markdown-preview-open-browser`. All websockets will be cleaned up on emacs termination. If you'd like to perform cleanup manually run `M-x markdown-preview-cleanup`.

## Multimarkdown

In order to enable multimarkdown support, customize
`M-x customize-option` -> `markdown-command` variable.
Look for `Markdown Command` which is set to `markdown` by default,
set to `multimarkdown` and make sure it's in your PATH.

## Browser

In order to select prefered browser, customize
`M-x customize-option` -> `browse-url-browser-function` option.
Select your browser from `Value menu`. If it's not there, follow [EmacsWiki: Browse Url](http://www.emacswiki.org/emacs/BrowseUrl).

## Theme

[Solarized-dark](http://thomasf.github.io/solarized-css/)

In order to change preview styling, run `M-x customize-option` -> `markdown-preview-style`
and specify a URL to your favourite markdown css file.

## Websocket port

Adjustable by `M-x customize-option` -> `markdown-preview-port`.

## el-get
* `M-x el-get-self-update`
* `M-x el-get-install` -> `markdown-preview-mode`

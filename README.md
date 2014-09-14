Markdown preview mode
===========================

## Description
Opens a preview in a browser, updated upon buffer save.
Same preview window is valid accross multiple mardown buffers.

## Dependencies

* markdown-mode.el
* websocket.el

Makes use of `markdown-mode`, which already can transform markdown into html
and `websocket.el` to deliver html to browser.

## Starting preview

`M-x markdown-preview-mode` will open preview in a browser and will start `markdown-mode` if it's not yet running for current buffer.

If you'v closed the preview window, you can start it over with
`M-x markdown-preview-open-browser`.


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

[Solarized-dark](https://github.com/thomasf/solarized-css)

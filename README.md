# hotfuzz

Approximate string matching completion style with a scoring algorithm
that factors in substring matches and word/path component/camelCase
boundaries.

To use hotfuzz, add it to the `completion-styles` list:
```elisp
(setq completion-styles '(hotfuzz))
```
or, if using [Selectrum], enable `hotfuzz-selectrum-mode`.

## Customization

Hotfuzz adheres to a few of the default Emacs completion style
configuration options:
* `completion-ignore-case` specifies whether case should be considered
  significant when matching.
* The face `completions-common-part` is used for highlighting the
  characters of a candidate that the search pattern matched.

[Selectrum]: https://github.com/raxod502/selectrum
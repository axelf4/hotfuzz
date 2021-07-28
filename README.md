# hotfuzz

[![test](https://github.com/axelf4/hotfuzz/actions/workflows/test.yml/badge.svg)](https://github.com/axelf4/hotfuzz/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/axelf4/hotfuzz/branch/master/graph/badge.svg?token=OV1BqTB7QL)](https://codecov.io/gh/axelf4/hotfuzz)

Approximate string matching completion style with a scoring algorithm
that factors in substring matches and word/path component/camelCase
boundaries.

To use hotfuzz, add it to the `completion-styles` list:
```elisp
(setq completion-styles '(hotfuzz))
```
or, if using [Selectrum], enable `hotfuzz-selectrum-mode`.

## Customization

Hotfuzz adheres to a few of the default Emacs completion configuration options:
* `completion-ignore-case` specifies whether case should be considered
  significant when matching.
* The face `completions-common-part` is used for highlighting the
  characters of a candidate that the search string matched.

## Related projects

### The `flex` completion style

The `flex` completion style included with Emacs
does the same matching as hotfuzz, and scores candidates based on gap sizes.
It does not, however, attempt to find the optimal score.
For example, given the search string `"foo"`,
the matched characters in a candidate could look like

> x**f**xxx**o**xxx**o**xfoox

which would score low even though
there is a contiguous match later in the string.

### flx

The [flx] package - which out-of-the-box only supports [Ido] -
has scoring criteria similar to those used by hotfuzz,
but works a little differently.
Its bountiful use of caching
means it can be faster at scoring long candidates.
Since the ordering of completions differs between flx and hotfuzz
you are encouraged to try both.

### orderless

The [orderless] completion style allows
every component of a space-delimited (by default) pattern
to match according to any other completion style.
It is very customizable,
but does no sorting and allows the individual sub-patterns to overlap
(`"foo foo"` filters no additional items compared to `"foo"`).
Hotfuzz on the other hand tries to be more *clever* about sorting,
and so users who dislike that may prefer orderless.

[Selectrum]: https://github.com/raxod502/selectrum
[flx]: https://github.com/lewang/flx
[Ido]: https://www.gnu.org/software/emacs/manual/html_node/ido/index.html
[orderless]: https://github.com/oantolin/orderless
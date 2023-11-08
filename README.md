# hotfuzz

[![test](https://github.com/axelf4/hotfuzz/actions/workflows/test.yml/badge.svg)](https://github.com/axelf4/hotfuzz/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/axelf4/hotfuzz/branch/master/graph/badge.svg?token=OV1BqTB7QL)](https://codecov.io/gh/axelf4/hotfuzz)
[![MELPA](https://melpa.org/packages/hotfuzz-badge.svg)](https://melpa.org/#/hotfuzz)

This is a fuzzy Emacs completion style similar to the built-in `flex` style,
but with a better scoring algorithm.
Specifically, it is non-greedy and ranks completions that match at
word; path component; or camelCase boundaries higher.

To use hotfuzz, add it to the `completion-styles` list:
```elisp
(setq completion-styles '(hotfuzz))
```
Or, if using [Fido], add hotfuzz to the `completion-styles` list this way:
```elisp
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (setq-local completion-styles '(hotfuzz))))
```

**Note:** Unless the completion UI supports the
`completion-lazy-hilit` variable, as i.a. [Vertico] and [Corfu] do,
then highlighting of the matched characters will only be applied to
the first `hotfuzz-max-highlighted-completions` completions, out of
performance concerns. The default value is large enough that generally
the list of completions will need to be scrolled beyond the second
page to reach non-highlighted completions. If you are annoyed by this
you can make it highlight all completions instead using
```elisp
(setq hotfuzz-max-highlighted-completions most-positive-fixnum)
```
provided you are completing small enough lists and/or do not encounter
performance problems.

## Customization

Hotfuzz adheres to a few of the default Emacs completion configuration options:
* `completion-ignore-case` specifies whether case should be considered
  significant when matching.
* The face `completions-common-part` is used for highlighting the
  characters of a candidate that the search string matched.

## Dynamic module

Optionally, you may compile the bundled dynamic module
to greatly improve the performance of filtering.
Once the shared object is available in `load-path`
it will automatically be picked up when hotfuzz is loaded,
or you may evaluate `(require 'hotfuzz-module)`
if hotfuzz already has been loaded.
To compile, make sure GCC, CMake and GNU Make or similar are present,
and run

```sh
mkdir build
cd build
cmake -DCMAKE_C_FLAGS='-O3 -march=native' .. \
	&& cmake --build .
```

and place the resulting shared library somewhere in `load-path`.

Unlike the Lisp implementation,
the dynamic module uses an unstable sorting algorithm.

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
Its bountiful use of caching means it can be faster at scoring long candidates.
Since the ordering of completions differs between flx and hotfuzz
you are encouraged to try both.

### fussy

The [fussy] completion style is generic over different fuzzy scoring backends,
flx and the Emacs Lisp implementation of hotfuzz being two of them.
fussy also implements caching of filtered candidates.
Although some of the scoring backends are implemented as dynamic modules,
hotfuzz has the advantage of passing all completion candidates
to its dynamic module en masse,
allowing sorting and filtering to be parallelized.

### orderless

The [orderless] completion style allows
every component of a space-delimited (by default) pattern
to match according to any other completion style.
It is very customizable,
but does no sorting and allows the individual sub-patterns to overlap
(`"foo foo"` filters no additional items compared to `"foo"`).
Hotfuzz on the other hand tries to be more clever about sorting,
and so users who dislike that may prefer orderless.

[Vertico]: https://github.com/minad/vertico
[Corfu]: https://github.com/minad/corfu
[Ido]: https://www.gnu.org/software/emacs/manual/html_node/ido/index.html
[Fido]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html
[flx]: https://github.com/lewang/flx
[fussy]: https://github.com/jojojames/fussy
[orderless]: https://github.com/oantolin/orderless

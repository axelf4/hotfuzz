# hotfuzz

[![test](https://github.com/axelf4/hotfuzz/actions/workflows/test.yml/badge.svg)](https://github.com/axelf4/hotfuzz/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/axelf4/hotfuzz/graph/badge.svg?token=OV1BqTB7QL)](https://codecov.io/gh/axelf4/hotfuzz)
[![MELPA](https://melpa.org/packages/hotfuzz-badge.svg)](https://melpa.org/#/hotfuzz)

This is a fuzzy Emacs completion style similar to the built-in `flex` style,
but with a better scoring algorithm.
Specifically, it is non-greedy and ranks completions that match at
word; path component; or camelCase boundaries higher.

To use hotfuzz, add it to the `completion-styles` list:
```elisp
(setq completion-styles '(hotfuzz))
```
If using [Fido], its insistence on using `flex` needs to be thwarted:
```elisp
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (kill-local-variable 'completion-styles)))
```

## Customization

The following ordinary Emacs completion options are adhered to:
* `completion-ignore-case` specifies whether matching is case-insignificant.
* The `completions-common-part` face is used to highlight
  what characters of a candidate the search string matched.

Unless the completion UI supports `completion-lazy-hilit`, as i.a.
[Vertico] and [Corfu] do, only the first
`hotfuzz-max-highlighted-completions` completions will be
highlighted out of performance concerns. The default value is large
enough that generally the list of completions will need to be
scrolled beyond the second page to reach non-highlighted
completions, but this optimization may be disabled with:
```elisp
(setq hotfuzz-max-highlighted-completions most-positive-fixnum)
```

## Dynamic module

Optionally, you may compile the bundled dynamic module
for improved performance.
Ensure GCC, CMake and GNU Make or similar are present, and run

```sh
cmake -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS=-march=native &&
	cmake --build build
```

and place the resulting shared library somewhere in `load-path`.
It will be automatically picked up,
or you may evaluate `(require 'hotfuzz-module)`
if hotfuzz has already been loaded.

Unlike the Lisp implementation,
the dynamic module uses an unstable sorting algorithm.

> [!NOTE]
> Dynamic modules are unable to access invalid Unicode strings.
>
> [Consult] appends invisible *tofus*, characters outside the Unicode
> range (unlikely to match search strings), to attach line numbers and
> disambiguate completions. Using e.g. the Supplementary Private Use
> Area-B instead circumvents encoding issues:
> ```elisp
> (setq consult--tofu-char #x100000
>       consult--tofu-range #x00fffe)
> ```

## Related projects

### The `flex` completion style

The `flex` completion style included with Emacs
does the same matching as hotfuzz, and scores candidates based on gap sizes.
It does not, however, attempt to find the optimal score.
For example, given the search string `"foo"`,
the matched characters in a candidate could look like

> x**f**xxx**o**xxx**o**xfoox

which would score low despite the later contiguous match.

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
[Consult]: https://github.com/minad/consult
[flx]: https://github.com/lewang/flx
[fussy]: https://github.com/jojojames/fussy
[orderless]: https://github.com/oantolin/orderless

;;; hotfuzz.el --- Fuzzy completion style  -*- lexical-binding: t -*-

;; Copyright (C) Axel Forsman

;; Author: Axel Forsman <axel@axelf.se>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: matching
;; URL: https://github.com/axelf4/hotfuzz
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a fuzzy Emacs completion style similar to the built-in
;; `flex' style, but with a better scoring algorithm. Specifically, it
;; is non-greedy and ranks completions that match at word; path
;; component; or camelCase boundaries higher.

;; To use this style, prepend `hotfuzz' to `completion-styles'.

;;; Code:

;; See: GOTOH, Osamu. An improved algorithm for matching biological
;;      sequences. Journal of molecular biology, 1982, 162.3: 705-708.

(eval-when-compile (require 'cl-lib))
(require 'hotfuzz-module nil t)
(declare-function hotfuzz--filter-c "hotfuzz-module")

(defgroup hotfuzz () "Fuzzy completion style." :group 'minibuffer)

(defcustom hotfuzz-max-highlighted-completions 25
  "The number of top-ranking completions that should be highlighted.
Large values will decrease performance."
  :type 'integer)

;; Pre-allocated vectors make the cost-only calculation optimization
;; where symmetricity w.r.t. insertions/deletions means it suffices to
;; allocate min(#needle, #haystack) for C/D inapplicable.
(defconst hotfuzz--max-needle-len 128)
(defconst hotfuzz--max-haystack-len 512)
(defvar hotfuzz--c (make-vector hotfuzz--max-needle-len 0))
(defvar hotfuzz--d (make-vector hotfuzz--max-needle-len 0))
(defvar hotfuzz--bonus (make-vector hotfuzz--max-haystack-len 0))

(defvar hotfuzz--filtering-p nil)

(defconst hotfuzz--bonus-lut
  (eval-when-compile
    (let ((state-special (make-char-table 'hotfuzz-bonus-lut 0))
          (state-upper (make-char-table 'hotfuzz-bonus-lut 0))
          (state-lower (make-char-table 'hotfuzz-bonus-lut 0))
          (word-bonus 80))
      (set-char-table-range state-upper '(?a . ?z) word-bonus)
      (cl-loop for (ch . bonus) in `((?/ . 90) (?. . 60) (?\  . ,word-bonus)
                                     (?- . ,word-bonus) (?_ . ,word-bonus))
               do (aset state-upper ch bonus) (aset state-lower ch bonus))
      (let ((lut (make-char-table 'hotfuzz-bonus-lut state-special)))
        (set-char-table-range lut '(?A . ?Z) state-upper)
        (set-char-table-range lut '(?a . ?z) state-lower)
        lut)))
  "LUT of the bonus associated with the current/previous characters.")

(defun hotfuzz--calc-bonus (haystack)
  "Precompute all potential bonuses for matching certain characters in HAYSTACK."
  (cl-loop for ch across haystack and i from 0 and lastch = ?/ then ch do
           (aset hotfuzz--bonus i (aref (aref hotfuzz--bonus-lut ch) lastch))))

;; Aᵢ denotes the prefix a₀,...,aᵢ₋₁ of A
(defun hotfuzz--match-row (a b i nc nd pc pd)
  "Calculate costs for transforming Aᵢ to Bⱼ with deletions for all j.
The matrix C[i][j] represents the minimum cost of a conversion, and D,
the minimum cost when aᵢ is deleted. The costs for row I are written
into NC/ND, using the row I-1 in PC/PD. The vectors NC/PC and ND/PD
respectively may alias."
  (cl-loop
   with m = (length b)
   and g = 100 and h = 10 ; Every k-symbol gap is penalized by g+hk
   ;; s threads the old value C[i-1][j-1] throughout the loop
   for j below m and s = (if (zerop i) 0 (+ g (* 5 i))) then oldc
   for oldc = (aref pc j) do
   ;; Either extend optimal conversion of (i) Aᵢ₋₁ to Bⱼ₋₁, by
   ;; matching bⱼ (C[i-1,j-1]-bonus); or (ii) Aᵢ₋₁ to Bⱼ, by deleting
   ;; aᵢ and opening a new gap (C[i-1,j]+g+h) or enlarging the
   ;; previous gap (D[i-1,j]+h).
   (aset nc j (min (aset nd j (+ h (min (+ oldc (if (< j (1- m)) g 0))
                                        (aref pd j))))
                   (if (char-equal (aref a i) (aref b j))
                       (- s (aref hotfuzz--bonus i))
                     most-positive-fixnum)))))

(defun hotfuzz--cost (needle haystack)
  "Return the difference score of NEEDLE and the match HAYSTACK."
  (let ((n (length haystack)) (m (length needle)))
    (if (> n hotfuzz--max-haystack-len)
        10000
      (hotfuzz--calc-bonus haystack)
      (let ((c (fillarray hotfuzz--c 10000)) (d (fillarray hotfuzz--d 10000)))
        (dotimes (i n) (hotfuzz--match-row haystack needle i c d c d))
        (aref c (1- m))))))

(defun hotfuzz-highlight (needle haystack)
  "Highlight destructively the characters NEEDLE matched in HAYSTACK.
HAYSTACK has to be a match according to `hotfuzz-all-completions'."
  (let ((n (length haystack)) (m (length needle))
        (case-fold-search completion-ignore-case))
    (unless (or (> n hotfuzz--max-haystack-len) (> m hotfuzz--max-needle-len))
      (hotfuzz--calc-bonus haystack)
      (cl-loop
       with rows initially
       (cl-loop for i below n and pc = (fillarray hotfuzz--c 10000) then nc
                and pd = (fillarray hotfuzz--d 10000) then nd
                and nc = (make-vector m 0) and nd = (make-vector m 0) do
                (hotfuzz--match-row haystack needle i nc nd pc pd)
                (push (cons nc nd) rows))
       ;; Backtrack to find matching positions
       for j from (1- m) downto 0 and i downfrom (1- n) do
       (cl-destructuring-bind (c . d) (pop rows)
         (when (<= (aref d j) (aref c j))
           (while (progn (setq i (1- i))
                         (> (aref d j) (aref (setq d (cdr (pop rows))) j))))))
       (add-face-text-property i (1+ i) 'completions-common-part nil haystack))))
  haystack)

(defmacro hotfuzz--dash-each (list &rest body)
  "Evaluate BODY for each element of LIST and return nil.
Each element of LIST in turn is bound to `it' and its index
within LIST to `it-index' before evaluating BODY.
This is the anaphoric counterpart to `-each'.

Shamelessly lifted from dash: https://github.com/magnars/dash.el"
  (let ((l (make-symbol "list"))
        (i (make-symbol "i")))
    `(let ((,l ,list)
           (,i 0))
       (while ,l
         (let ((it (pop ,l)) (it-index ,i))
           (ignore it it-index)
           ,@body)
         (setq ,i (1+ ,i))))))

(defmacro hotfuzz--dash-keep (form list)
  "Eval FORM for each item in LIST and return the non-nil results.
Like `--filter', but returns the non-nil results of FORM instead
of the corresponding elements of LIST.  Each element of LIST in
turn is bound to `it' and its index within LIST to `it-index'
before evaluating FORM.
This is the anaphoric counterpart to `-keep'.

Shamelessly lifted from dash: https://github.com/magnars/dash.el"
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (hotfuzz--dash-each ,list (let ((,m ,form)) (when ,m (push ,m ,r))))
       (nreverse ,r))))

(defun hotfuzz--fix-tofu-chars (orig-fun string candidates &optional ignore-case)
  "Workaround tofu chars (in e.g. consult) for native module filtering."
  (let*
      ((table (make-hash-table :test #'eq :size (length candidates)))
       (cands
        (hotfuzz--dash-keep
         (when (stringp it)
           (let ((encoded (encode-coding-string it 'no-conversion 'nocopy)))
             (setf (gethash encoded table) it)
             (and (< (length encoded) hotfuzz--max-haystack-len) encoded)))
         candidates))
       (raw-str (encode-coding-string string 'no-conversion 'nocopy))
       (ans
        (let
            ((gc-cons-threshold most-positive-fixnum)
             (gc-cons-percentage 1.0))
          (funcall orig-fun raw-str cands ignore-case))))
    (hotfuzz--dash-keep (gethash it table) ans)))

;;;###autoload
(defun hotfuzz-all-completions (string table &optional pred point)
  "Get hotfuzz-completions of STRING in TABLE.
See `completion-all-completions' for the semantics of PRED and POINT.
This function prematurely sorts the completions; mutating the result
before passing it to `display-sort-function' or `cycle-sort-function'
will lead to inaccuracies."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (if point (substring string point) ""))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (needle (substring beforepoint (car bounds)))
         (use-module-p (fboundp 'hotfuzz--filter-c))
         (case-fold-search completion-ignore-case)
         (completion-regexp-list
          (if use-module-p completion-regexp-list
            (let ((re (mapconcat
                       (lambda (ch) (let ((s (char-to-string ch)))
                                      (concat "[^" s "]*" (regexp-quote s))))
                       needle "")))
              (cons (concat "\\`" re) completion-regexp-list))))
         (all (if (and (string= prefix "") (stringp (car-safe table))
                       (not (or pred completion-regexp-list (string= needle ""))))
                  table (all-completions prefix table pred))))
    ;; `completion-pcm--all-completions' tests completion-regexp-list
    ;; again with functional tables even though they should handle it.
    (cond
     ((or (null all) (string= needle "")))
     (use-module-p (setq all (hotfuzz--filter-c needle all completion-ignore-case)))
     ((> (length needle) hotfuzz--max-needle-len))
     (t (cl-loop for x in-ref all do (setf x (cons (hotfuzz--cost needle x) x)))
        (cl-loop for y in-ref (setq all (sort all #'car-less-than-car))
                 do (setf y (cdr y)))))
    (setq hotfuzz--filtering-p (not (string= needle "")))
    (defvar completion-lazy-hilit-fn) ; Introduced in Emacs 30 (bug#47711)
    (if (bound-and-true-p completion-lazy-hilit)
        (setq completion-lazy-hilit-fn (apply-partially #'hotfuzz-highlight needle))
      (cl-loop repeat hotfuzz-max-highlighted-completions and for x in-ref all
               do (setf x (hotfuzz-highlight needle (copy-sequence x)))))
    (and all (if (string= prefix "") all (nconc all (length prefix))))))

;;;###autoload
(defun hotfuzz--adjust-metadata (metadata)
  "Adjust completion METADATA for hotfuzz sorting."
  (if hotfuzz--filtering-p
      `(metadata (display-sort-function . identity) (cycle-sort-function . identity)
                 . ,(cdr metadata))
    metadata))

;;;###autoload
(progn
  (add-to-list 'completion-styles-alist
               '(hotfuzz completion-flex-try-completion hotfuzz-all-completions
                         "Fuzzy completion."))
  ;; Why is the Emacs completion API so cursed?
  (put 'hotfuzz 'completion--adjust-metadata #'hotfuzz--adjust-metadata))

(provide 'hotfuzz)
;;; hotfuzz.el ends here

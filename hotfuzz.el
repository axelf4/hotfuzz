;;; hotfuzz.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Axel Forsman

;; Author: Axel Forsman <axelsfor@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: matching
;; Homepage: https://github.com/axelf4/hotfuzz
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This is a fuzzy Emacs completion style similar to the built-in
;; `flex' style, but with a better scoring algorithm. Specifically, it
;; is non-greedy and ranks completions that match at word; path
;; component; or camelCase boundaries higher.

;; To use this style, prepend `hotfuzz' to `completion-styles'.

;;; Code:

;; See: Myers, Eugene W., and Webb Miller. "Optimal alignments in
;;      linear space." Bioinformatics 4.1 (1988): 11-17.

(eval-when-compile (require 'cl-lib))

(defgroup hotfuzz nil
  "Fuzzy completion style."
  :group 'minibuffer
  :link '(url-link :tag "GitHub" "https://github.com/axelf4/hotfuzz"))

(defcustom hotfuzz-max-highlighted-completions 25
  "The number of top-ranking completions that should be highlighted.
Large values will decrease performance. Only applies when using the
Emacs `completion-styles' interface."
  :group 'hotfuzz
  :type 'integer)

(declare-function hotfuzz--filter-c "hotfuzz-module")
;; If the dynamic module is available: Load it
(require 'hotfuzz-module nil t)

;; Since we pre-allocate the vectors the common optimization where
;; symmetricity w.r.t. to insertions/deletions means it suffices to
;; allocate min(#needle, #haystack) for C/D when only calculating the
;; cost does not apply.
(defconst hotfuzz--max-needle-len 128)
(defvar hotfuzz--c (make-vector hotfuzz--max-needle-len 0))
(defvar hotfuzz--d (make-vector hotfuzz--max-needle-len 0))
(defvar hotfuzz--bonus (make-vector 512 0))

(defconst hotfuzz--bonus-prev-luts
  (eval-when-compile
    (let ((bonus-state-special (make-char-table 'hotfuzz-bonus-lut 0))
          (bonus-state-upper (make-char-table 'hotfuzz-bonus-lut 0))
          (bonus-state-lower (make-char-table 'hotfuzz-bonus-lut 0))
          (word-bonus 80))
      (cl-loop for (ch . bonus) in `((?/ . 90) (?. . 60)
                                     (?- . ,word-bonus) (?_ . ,word-bonus)
                                     (?\  . ,word-bonus))
               do (aset bonus-state-upper ch bonus) (aset bonus-state-lower ch bonus))
      (cl-loop for ch from ?a to ?z do (aset bonus-state-upper ch word-bonus))
      (vector bonus-state-special bonus-state-upper bonus-state-lower)))
  "LUTs of the bonus associated with the previous character.")
(defconst hotfuzz--bonus-cur-lut
  (eval-when-compile
    (let ((bonus-cur-lut (make-char-table 'hotfuzz-bonus-lut 0)))
      (cl-loop for ch from ?A to ?Z do (aset bonus-cur-lut ch 1))
      (cl-loop for ch from ?a to ?z do (aset bonus-cur-lut ch 2))
      bonus-cur-lut))
  "LUT of the `hotfuzz--bonus-prev-luts' index based on the current character.")

(defun hotfuzz--calc-bonus (haystack)
  "Precompute all potential bonuses for matching certain characters in HAYSTACK."
  (cl-loop for ch across haystack and i = 0 then (1+ i) and lastch = ?/ then ch do
           (aset hotfuzz--bonus i
                 (aref (aref hotfuzz--bonus-prev-luts (aref hotfuzz--bonus-cur-lut ch)) lastch))))

;; Aᵢ denotes the prefix a₀,...,aᵢ₋₁ of A
(defun hotfuzz--match-row (a b i nc nd pc pd)
  "Calculate costs for transforming Aᵢ to Bⱼ with deletions for all j.
The matrix C[i][j] represents the minimum cost of a conversion, and D,
the minimum cost when aᵢ is deleted. The costs for row I are written
into NC/ND, using the costs for row I-1 in PC/PD. The vectors NC/PC
and ND/PD respectively may alias."
  (cl-loop
   with m = (length b) and oldc
   and g = 100 and h = 5 ; Every k-symbol gap is penalized by g+hk
   ;; s threads the old value C[i-1][j-1] throughout the loop
   for j below m and s = (if (zerop i) 0 (+ g (* h i))) then oldc do
   (setq oldc (aref pc j))
   ;; Either extend optimal conversion of (i) Aᵢ₋₁ to Bⱼ₋₁, by
   ;; matching bⱼ (C[i-1,j-1]-bonus); or (ii) Aᵢ₋₁ to Bⱼ, by deleting
   ;; aᵢ and opening a new gap (C[i-1,j]+g+h) or enlarging the
   ;; previous gap (D[i-1,j]+h).
   (aset nc j (min (aset nd j (+ (min (aref pd j) (+ oldc g))
                                 (if (= j (1- m)) h (* 2 h))))
                   (if (char-equal (aref a i) (aref b j))
                       (- s (aref hotfuzz--bonus i))
                     most-positive-fixnum)))))

(defun hotfuzz--cost (needle haystack)
  "Return the difference score of NEEDLE and the match HAYSTACK."
  (let ((n (length haystack)) (m (length needle))
        (c hotfuzz--c) (d hotfuzz--d))
    (fillarray c 10000)
    (fillarray d 10000)
    (hotfuzz--calc-bonus haystack)
    (dotimes (i n) (hotfuzz--match-row haystack needle i c d c d))
    (aref c (1- m)))) ; Final cost

(defun hotfuzz-highlight (needle haystack)
  "Highlight the characters that NEEDLE matched in HAYSTACK.
HAYSTACK has to be a match according to `hotfuzz-filter'."
  (let ((n (length haystack)) (m (length needle))
        (c hotfuzz--c) (d hotfuzz--d)
        (case-fold-search completion-ignore-case))
    (if (> m hotfuzz--max-needle-len)
        haystack ; Bail out if search string is too long
      (fillarray c 10000)
      (fillarray d 10000)
      (hotfuzz--calc-bonus haystack)
      (cl-loop
       with rows = (cl-loop
                    with nc and nd
                    for i below n and pc = c then nc and pd = d then nd with res = nil do
                    (setq nc (make-vector m 0) nd (make-vector m 0))
                    (hotfuzz--match-row haystack needle i nc nd pc pd)
                    (push `(,nc . ,nd) res)
                    finally return res)
       ;; Backtrack to find matching positions
       for j from (1- m) downto 0 with i = n do
       (when (<= (aref (cdar rows) j) (aref (caar rows) j))
         (while (cl-destructuring-bind (_c . d) (pop rows)
                  (cl-decf i)
                  (and (> i 0) (< (aref (cdar rows) j) (aref d j))))))
       (pop rows)
       (cl-decf i)
       (add-face-text-property i (1+ i) 'completions-common-part nil haystack)
       finally return haystack))))

;;;###autoload
(cl-defun hotfuzz-filter (string candidates &optional (start 0))
  "Filter CANDIDATES that match STRING and sort by the match costs.
CANDIDATES should be a list of strings. If START is non-nil, the first
START characters of each candidate string are ignored."
  (cond
   ((or (string= string "") (> (length string) hotfuzz--max-needle-len))
    candidates)
   ((and (featurep 'hotfuzz-module) (= start 0))
    (hotfuzz--filter-c string candidates))
   ((let ((re (concat
               "\\`"
               (when (> start 0) (format ".\\{%d\\}" start))
               (mapconcat
                (lambda (ch) (format "[^%c]*%s" ch (regexp-quote (char-to-string ch))))
                string "")))
          (case-fold-search completion-ignore-case))
      (mapcar
       #'car
       (cl-sort
        (cl-loop for x in candidates if (string-match-p re x)
                 collect (cons x (hotfuzz--cost string (if (> start 0) (substring x start) x))))
        #'< :key #'cdr))))))

;;; Completion style implementation

;;;###autoload
(defun hotfuzz-all-completions (string table pred point)
  "Implementation of `completion-all-completions' that uses hotfuzz.
This function prematurely sorts the completions; mutating the returned
list before passing it to `display-sort-function' or
`cycle-sort-function' will lead to inaccuracies."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (needle (substring beforepoint (car bounds)))
         (completion-regexp-list nil)
         (all (hotfuzz-filter
               needle
               (if (and (listp table) (not (consp (car-safe table)))
                        (not pred) (string= prefix ""))
                   table
                 (all-completions prefix table pred))
               (length prefix))))
    (when (and (not (string= needle "")) all)
      ;; Highlighting all completions without deferred highlighting
      ;; (bug#47711) would take too long.
      (cl-loop
       repeat hotfuzz-max-highlighted-completions and for x in-ref all do
       (setf x (concat prefix
                       (hotfuzz-highlight needle (substring x (length prefix))))))
      (unless (> hotfuzz-max-highlighted-completions 0)
        (setcar all (copy-sequence (car all))))
      (put-text-property 0 1 'completion-sorted t (car all)))
    (if (string= prefix "") all (nconc all (length prefix)))))

(defun hotfuzz--adjust-metadata (metadata)
  "Adjust completion METADATA for hotfuzz sorting."
  (let ((existing-dsf (completion-metadata-get metadata 'display-sort-function))
        (existing-csf (completion-metadata-get metadata 'cycle-sort-function)))
    (cl-flet
        ((compose-sort-fn
          (existing-sort-fn)
          (lambda (completions)
            (if (or (null completions)
                    (get-text-property 0 'completion-sorted (car completions)))
                completions
              (funcall existing-sort-fn completions)))))
      `(metadata
        (display-sort-function . ,(compose-sort-fn (or existing-dsf #'identity)))
        (cycle-sort-function . ,(compose-sort-fn (or existing-csf #'identity)))
        ,@(cdr metadata)))))

;;;###autoload
(progn
  ;; Why is the Emacs completions API so cursed?
  (put 'hotfuzz 'completion--adjust-metadata #'hotfuzz--adjust-metadata)
  (add-to-list 'completion-styles-alist
               '(hotfuzz completion-flex-try-completion hotfuzz-all-completions
                         "Fuzzy completion.")))

;;; Selectrum integration

(defun hotfuzz--highlight-all (string candidates)
  "Highlight where STRING matches in the elements of CANDIDATES."
  (mapcar (lambda (candidate)
            (hotfuzz-highlight string (copy-sequence candidate)))
          candidates))

(defvar selectrum-refine-candidates-function)
(defvar selectrum-highlight-candidates-function)

(defvar hotfuzz--prev-selectrum-functions nil
  "Previous values of the Selectrum sort/filter/highlight API endpoints.")

;;;###autoload
(progn
  (define-minor-mode hotfuzz-selectrum-mode
    "Minor mode that enables hotfuzz in Selectrum menus."
    :group 'hotfuzz
    :global t
    (if hotfuzz-selectrum-mode
        (setq hotfuzz--prev-selectrum-functions
              `(,(when (boundp 'selectrum-refine-candidates-function)
                   selectrum-refine-candidates-function)
                . ,(when (boundp 'selectrum-highlight-candidates-function)
                     selectrum-highlight-candidates-function))
              selectrum-refine-candidates-function #'hotfuzz-filter
              selectrum-highlight-candidates-function #'hotfuzz--highlight-all)
      (cl-flet ((restore
                 (sym old our &aux (standard (car-safe (get sym 'standard-value))))
                 (cond ((not (eq (symbol-value sym) our)))
                       (old (set sym old))
                       (standard (set sym (eval standard t)))
                       (t (makunbound sym)))))
        (cl-destructuring-bind (old-rcf . old-hcf) hotfuzz--prev-selectrum-functions
          (restore 'selectrum-refine-candidates-function old-rcf #'hotfuzz-filter)
          (restore 'selectrum-highlight-candidates-function old-hcf #'hotfuzz--highlight-all))))))

(provide 'hotfuzz)
;;; hotfuzz.el ends here

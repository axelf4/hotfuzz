;;; hotfuzz.el --- Fuzzy completion style  -*- lexical-binding: t; -*-

;; Copyright 2021 Axel Forsman

;; Author: Axel Forsman <axelsfor@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") cl-lib)
;; Keywords: matching
;; Homepage: https://github.com/axelf4/hotfuzz
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Approximate string matching completion style with a scoring
;; algorithm that factors in substring matches and word/path
;; component/camelCase boundaries.

;; See: Myers, Eugene W., and Webb Miller. "Optimal alignments in
;;      linear space." Bioinformatics 4.1 (1988): 11-17.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Since we pre-allocate the vectors the common optimization where
;; symmetricity w.r.t. to insertions/deletions means it suffices to
;; allocate MIN(#needle, #haystack) for C/D when only calculating the
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
the minimum cost when aᵢ is deleted. The costs for row i are written
into NC/ND, using the costs for row i-1 in PC/PD. The vectors NC/PC
and ND/PD respectively may alias."
  (cl-loop
   with oldc
   and g = 100 and h = 5 ; Every k-symbol gap is penalized by g+hk
   ;; s threads the old value C[i-1][j-1] throughout the loop
   for j below (length b) and s = (if (zerop i) 0 (+ g (* h i))) then oldc do
   (setq oldc (aref pc j))
   ;; Either extend optimal conversion of (i) Aᵢ₋₁ to Bⱼ₋₁, by
   ;; matching bⱼ (C[i-1,j-1]-bonus); or (ii) Aᵢ₋₁ to Bⱼ, by deleting
   ;; aᵢ and opening a new gap (C[i-1,j]+g+h) or enlarging the
   ;; previous gap (D[i-1,j]+h).
   (aset nc j (min (aset nd j (+ (min (aref pd j) (+ oldc g)) h))
                   (if (char-equal (aref a i) (aref b j))
                       (- s (aref hotfuzz--bonus i))
                     most-positive-fixnum)))))

(defun hotfuzz--cost (needle haystack)
  (let ((n (length needle)) (m (length haystack))
        (c hotfuzz--c) (d hotfuzz--d))
    (fillarray c 10000)
    (fillarray d 10000)
    (hotfuzz--calc-bonus haystack)
    (dotimes (i m) (hotfuzz--match-row haystack needle i c d c d))
    (aref c (1- n)))) ; Final cost

;;;###autoload
(defun hotfuzz-filter (string candidates)
  "Filter CANDIDATES that match STRING and sort by the match costs."
  (if (or (> (length string) hotfuzz--max-needle-len) (string= string ""))
      candidates
    (let ((re (concat "^" (mapconcat (lambda (char)
                                       (format "[^%c]*%s"
                                               char
                                               (regexp-quote (char-to-string char))))
                                     string "")))
          (case-fold-search completion-ignore-case))
      (sort (cl-loop for x in candidates if (string-match re x) do
                     (setq x (copy-sequence x))
                     (put-text-property 0 1 'completion-cost (hotfuzz--cost string x) x)
                     and collect x)
            (lambda (a b) (< (get-text-property 0 'completion-cost a)
                             (get-text-property 0 'completion-cost b)))))))

(defun hotfuzz-highlight (needle haystack)
  "Highlight the characters that NEEDLE matched in HAYSTACK.

HAYSTACK has to be a match according to `hotfuzz-filter'."
  (let ((n (length needle)) (m (length haystack))
        (c hotfuzz--c) (d hotfuzz--d)
        (case-fold-search completion-ignore-case))
    (if (> n hotfuzz--max-needle-len)
        haystack ; Bail out if search string is too long
      (fillarray c 10000)
      (fillarray d 10000)
      (hotfuzz--calc-bonus haystack)
      (cl-loop
       with rows = (cl-loop
                    with nc = nil and nd = nil
                    for i below m and pc = c then nc and pd = d then nd with res = nil do
                    (setq nc (make-vector n 0) nd (make-vector n 0))
                    (hotfuzz--match-row haystack needle i nc nd pc pd)
                    (push `(,nc . ,nd) res)
                    finally return res)
       ;; Backtrack to find matching positions
       for j from (1- n) downto 0 with i = m do
       (when (<= (aref (cdar rows) j) (aref (caar rows) j))
         (while (cl-destructuring-bind (_c . d) (pop rows)
                  (cl-decf i)
                  (and (> i 0) (< (aref (cdar rows) j) (aref d j))))))
       (pop rows)
       (cl-decf i)
       (add-face-text-property i (1+ i) 'completions-common-part nil haystack)
       finally return haystack))))

;;; Completion style implementation

;; Without deferred highlighting (bug#47711) we do not even make an attempt
;;;###autoload
(defun hotfuzz--all-completions (string table pred point)
  "Implementation of `completion-all-completions' that uses hotfuzz."
  (pcase-let ((`(,all ,_pattern ,prefix ,_suffix ,_carbounds)
               (completion-substring--all-completions
                string table pred point
                #'completion-flex--make-flex-pattern))
              (case-fold-search completion-ignore-case))
    (when all
      (nconc (if (or (> (length string) hotfuzz--max-needle-len) (string= string ""))
                 all
               (mapcar (lambda (x)
                         (setq x (copy-sequence x))
                         (put-text-property 0 1 'completion-score (- (hotfuzz--cost string x)) x)
                         x)
                       all))
             (length prefix)))))

;;;###autoload
(progn
  ;; Why is the Emacs completions API so cursed?
  (put 'hotfuzz 'completion--adjust-metadata #'completion--flex-adjust-metadata)
  (add-to-list 'completion-styles-alist
               '(hotfuzz completion-flex-try-completion hotfuzz--all-completions)))

;;; Selectrum integration

(defun hotfuzz--highlight-all (string candidates)
  "Highlight where STRING matches in the elements of CANDIDATES."
  (mapcar (lambda (candidate)
            (hotfuzz-highlight string (copy-sequence candidate)))
          candidates))

(defvar selectrum-refine-candidates-function)
(defvar selectrum-highlight-candidates-function)

;;;###autoload
(progn
  (defvar hotfuzz--prev-selectrum-functions nil
    "Previous values of the Selectrum sort/filter/highlight API endpoints.")

  (define-minor-mode hotfuzz-selectrum-mode
    "Minor mode that enables hotfuzz in Selectrum menus."
    :global t
    (require 'selectrum)
    (if hotfuzz-selectrum-mode
        (setq hotfuzz--prev-selectrum-functions
              `(,selectrum-refine-candidates-function
                . ,selectrum-highlight-candidates-function)
              selectrum-refine-candidates-function #'hotfuzz-filter
              selectrum-highlight-candidates-function #'hotfuzz--highlight-all)
      (pcase hotfuzz--prev-selectrum-functions
        (`(,old-refine . ,old-highlight)
         (when (eq selectrum-refine-candidates-function #'hotfuzz-filter)
           (setq selectrum-refine-candidates-function old-refine))
         (when (eq selectrum-highlight-candidates-function #'hotfuzz--highlight-all)
           (setq selectrum-highlight-candidates-function old-highlight)))))))

(provide 'hotfuzz)
;;; hotfuzz.el ends here

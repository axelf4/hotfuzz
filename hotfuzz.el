;;; hotfuzz.el --- Fuzzy completion style  -*- lexical-binding: t; -*-
;;; See: Optimal alignments in linear space

(eval-when-compile (require 'cl-lib))

;; Since we pre-allocate the vectors the common optimization where
;; symmetricity w.r.t. to insertions/deletions means it suffices to
;; allocate MIN(#needle, #haystack) for c/d when only calculating the
;; score does not apply.
(defconst hotfuzz-max-match-len 128)
(defvar hotfuzz--c (make-vector hotfuzz-max-match-len 0))
(defvar hotfuzz--d (make-vector hotfuzz-max-match-len 0))
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
  "LUTs of the bonus associated with the previous character, depending
on the current character state.")
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

(defun hotfuzz--match-row (a b i nc nd pc pd)
  "Calculate costs for transforming Aᵢ to Bⱼ with deletions for all j.

The vectors nc/pc and nd/pd respectively may alias.

needle: B
haystack: A
i - the row
j - the column"
  (cl-loop
   with oldc
   and g = 100 and h = 5 ; Every k-symbol gap is penalized by g+hk
   ;; s threads the old value C[i-1][j-1] throughout the loop
   for j below (length b) and s = (if (zerop i) 0 (+ g (* h i))) then oldc do
   (setq oldc (aref pc j))
   (aset nc j (min (aset nd j (+ (min (aref pd j) (+ oldc g)) h))
                   (if (char-equal (aref a i) (aref b j))
                       (- s (aref hotfuzz--bonus i))
                     most-positive-fixnum)))))

(defun hotfuzz--score (needle haystack)
  (let ((n (length needle)) (m (length haystack))
        (c hotfuzz--c) (d hotfuzz--d))
    (fillarray c 10000)
    (fillarray d 10000)
    (hotfuzz--calc-bonus haystack)
    (dotimes (i m) (hotfuzz--match-row haystack needle i c d c d))
    (aref c (1- n)))) ; Final cost

;;;###autoload
(defun hotfuzz-filter (string candidates)
  "Filter CANDIDATES that match STRING and sort by the match scores."
  (if (or (> (length string) hotfuzz--max-match-len) (string-empty-p string))
      candidates
    (let ((re (mapconcat (lambda (char) (format "[^%1$s]*%1$s"
                                                (regexp-quote (char-to-string char))))
                         string ""))
          (case-fold-search t))
      (sort (cl-loop for x in candidates if (string-match re x) do
                     (setq x (copy-sequence x))
                     (put-text-property 0 1 'completion-score (hotfuzz--score string x) x)
                     and collect x)
            (lambda (a b) (< (get-text-property 0 'completion-score a)
                             (get-text-property 0 'completion-score b)))))))

(defun hotfuzz-highlight (needle haystack)
  "Highlight the characters that NEEDLE matched in HAYSTACK."
  (let ((n (length needle)) (m (length haystack))
        (c hotfuzz--c) (d hotfuzz--d)
        (case-fold-search t))
    (if (> n hotfuzz--max-match-len)
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
       ;; Backtrack to find optimal matching positions
       for j from (1- n) downto 0 with i = m do
       (while (cl-destructuring-bind (c . d) (pop rows)
                (cl-decf i)
                (<= (aref d j) (aref c j))))
       (add-face-text-property i (1+ i) 'completions-common-part nil haystack)
       finally return haystack))))

(provide 'hotfuzz)

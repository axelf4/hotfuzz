;;; hotfuzz.el --- Completion style -*- lexical-binding: t; -*-
;;; See: Optimal alignments in linear space

(eval-when-compile (require 'cl-lib))

(defconst hotfuzz-g 100)
(defconst hotfuzz-h 10)

;; Since we pre-allocate the vectors the common optimization where
;; symmetricity w.r.t. to insertions/deletions means it suffices to
;; allocate MIN(#needle, #haystack) for c/d when only calculating the
;; score does not apply.
(defconst hotfuzz-max-match-len 128)
(defvar hotfuzz--c (make-vector hotfuzz-max-match-len 0))
(defvar hotfuzz--d (make-vector hotfuzz-max-match-len 0))

(defun hotfuzz--match-row (a b i nc nd pc pd)
  "The inner loop.

The vectors nc/pc and nd/pd respectively may alias.

needle: B
haystack: A
i - the row
j - the column"
  (cl-loop
   with oldc = (if (zerop i) 0 (+ hotfuzz-g (* hotfuzz-h i)))
   ;; s threads the old value C[i-1][j-1] throughout the loop
   for j below (length b) and s = oldc then oldc do
   (setq oldc (aref pc j))
   (aset nc j (min (aset nd j (+ (min (aref pd j) (+ oldc hotfuzz-g)) hotfuzz-h))
                   (if (char-equal (aref a i) (aref b j))
                       s
                     most-positive-fixnum)))))

(defun hotfuzz--score (needle haystack)
  (let* ((n (length needle)) (m (length haystack))
         (c hotfuzz--c) (d hotfuzz--d))
    (if (> m hotfuzz-max-match-len)
        most-positive-fixnum
      (cl-loop for j below n do (aset d j (aset c j 10000)))
      (cl-loop for i below m do (hotfuzz--match-row haystack needle i c d c d)
               finally return (if (zerop n)
                                  (+ hotfuzz-g (* hotfuzz-h m))
                                (aref c (1- n))))))) ; Final cost

;;;###autoload
(defun hotfuzz-filter (string candidates)
  ""
  (if (string-empty-p string)
      candidates
    (let ((re (mapconcat (lambda (char) (format "[^%1$s]*%1$s"
                                                (regexp-quote (char-to-string char))))
                         string "")))
      (sort (cl-loop for x in candidates
                     if (string-match re x)
                     do (setq x (copy-sequence x))
                     (put-text-property 0 1 'completion-score (hotfuzz--score string x) x)
                     and collect x)
            (lambda (a b) (< (get-text-property 0 'completion-score a)
                             (get-text-property 0 'completion-score b)))))))

(defun hotfuzz-highlight (needle haystack)
  "Highlight the characters that NEEDLE matched in HAYSTACK."
  (let* ((n (length needle)) (m (length haystack))
         (c hotfuzz--c) (d hotfuzz--d))
    (if (> m hotfuzz-max-match-len)
        haystack ; Bail out if too long candidate
      (cl-loop for j below n do (aset d j (aset c j 10000)))
      (let ((rows (cl-loop
                   with nc = nil and nd = nil
                   for i below m and pc = c then nc and pd = d then nd with res = nil do
                   (setq nc (make-vector n 0) nd (make-vector n 0))
                   (hotfuzz--match-row haystack needle i nc nd pc pd)
                   (push `(,nc . ,nd) res)
                   finally return res)))
        ;; Backtrack to find optimal matching positions
        (cl-loop for j from (1- n) downto 0 with i = m do
                 (while (cl-destructuring-bind (c . d) (progn (cl-decf i)
                                                              (pop rows))
                          (<= (aref d j) (aref c j))))
                     (add-face-text-property i (1+ i) 'completions-common-part nil haystack)
                 finally return haystack)))))

(provide 'hotfuzz)

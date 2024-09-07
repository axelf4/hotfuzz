;;; tests.el --- The hotfuzz test suite  -*- lexical-binding: t -*-

(require 'ert)
(require 'hotfuzz)

;;; Validation of costs of preferable traits, all else being equal

(ert-deftest shorter-match-cost-test ()
  "Shorter matches should be given lower costs than longer ones."
  (should (< (hotfuzz--cost "b" "abc") (hotfuzz--cost "b" "abcd"))))

(ert-deftest bos-match-cost-test ()
  "Matches at the beginning are preferred."
  (should (< (hotfuzz--cost "a" "ab") (hotfuzz--cost "a" "ba"))))

(ert-deftest substring-match-cost-test ()
  "A substring match means fewer gaps and lower cost."
  (should (< (hotfuzz--cost "ab" "abcd") (hotfuzz--cost "ab" "acbd"))))

(ert-deftest camelcase-match-cost-test ()
  (should (< (hotfuzz--cost "ac" "AbCd") (hotfuzz--cost "ac" "abcd"))))

(ert-deftest special-match-cost-test ()
  (should (<= (hotfuzz--cost "x" "/x")
              (hotfuzz--cost "x" "-x")
              (hotfuzz--cost "x" " x")
              (hotfuzz--cost "x" ".x")
              (hotfuzz--cost "x" "yx"))))

(ert-deftest tighter-match-cost-test ()
  "Test that matches spanning fewer characters are better."
  (should (< (hotfuzz--cost "ab" "xaxbxx") (hotfuzz--cost "ab" "xaxxbx"))))

;;; Highlighting tests

(ert-deftest highlight-optimal-test ()
  "Test that the algorithm is non-greedy."
  (should (ert-equal-including-properties
           (hotfuzz-highlight "ab" "xaxbxabxaxbx")
           #("xaxbxabxaxbx" 5 7 (face completions-common-part)))))

;;; Filtering tests

(ert-deftest case-sensitivity-test ()
  (let ((xs '("aa" "aA " "Aa  " "AA   ")))
    (let ((completion-ignore-case nil))
      (should (equal (hotfuzz-all-completions "a" xs) '("aa" "aA " "Aa  ")))
      (should (equal (hotfuzz-all-completions "A" xs) '("Aa  " "AA   " "aA "))))
    (let ((completion-ignore-case t))
      (should (equal (hotfuzz-all-completions "a" xs) xs))
      (should (equal (hotfuzz-all-completions "A" xs) xs)))))

(ert-deftest long-candidates-test ()
  (let ((a (make-string 4096 ?x))
        (b (concat (make-string 2047 ?y) "x" (make-string 2048 ?y))))
    ;; Too long candidates should still be filtered with matches
    ;; lumped together at the end in their original order.
    (should (equal (hotfuzz-all-completions "x" (list (make-string 4096 ?y) b a "x"))
                   (list "x" b a)))))

(ert-deftest filter-long-needle-test ()
  (let* ((needle (make-string (1+ hotfuzz--max-needle-len) ?x))
         (a (concat needle "y")))
    ;; With a too long search string candidates should only be
    ;; filtered but not sorted.
    (should (equal (hotfuzz-all-completions needle (list a "y" needle))
                   (list a needle)))))

(ert-deftest all-completions-test ()
  (let* ((completion-styles '(hotfuzz))
         (s "fb")
         (table '("foobar" "fxxx" "foo-baz" "" "fb"))
         (meta (completion-metadata s table nil))
         (candidates (completion-all-completions s table nil (length s) meta))
         (sort-fn (alist-get 'display-sort-function meta))
         (last (last candidates)))
    (when (numberp (cdr last)) (setcdr last nil))
    (when sort-fn (setq candidates (funcall sort-fn candidates)))
    ;; Completions should be eagerly fontified by default
    (should (equal-including-properties
             candidates
             '(#("fb" 0 2 (face completions-common-part))
               #("foo-baz" 0 1 (face completions-common-part) 4 5 (face completions-common-part))
               #("foobar" 0 1 (face completions-common-part) 3 4 (face completions-common-part)))))))

(ert-deftest display-sort-function-test ()
  "Test that empty strings apply the completion function `display-sort-function'."
  (cl-flet ((sorted-completions (string)
              (let* ((completion-styles '(hotfuzz))
                     (table '("xbbx" "xx" "xax"))
                     (md `(metadata (display-sort-function
                                     . ,(lambda (xs) (sort xs #'string<)))))
                     (all (completion-all-completions
                           string table nil (length string) md)))
                (funcall (alist-get 'display-sort-function md) all))))
    (should (equal (sorted-completions "") '("xax" "xbbx" "xx"))) ; Lexicographically sorted
    (should (equal (sorted-completions "xx") '("xx" "xax" "xbbx")))))

(ert-deftest boundaries-test ()
  "Test completion on a single field of a filename."
  (let ((completion-styles '(hotfuzz)))
    (should
     (equal
      (completion-all-completions
       "/usr/s/man"
       (lambda (string pred action)
         (let ((dir (file-name-directory string)))
           (pcase action
             (`(boundaries . ,suffix)
              `(boundaries ,(length dir) . ,(string-match-p "/" suffix)))
             ('t (all-completions "" '("bin/" "share/" "local/") pred)))))
       nil
       6) ; Point as in "/usr/s|/man"
      '("share/" . 5)))))

(defvar completion-lazy-hilit)
(defvar completion-lazy-hilit-fn)
(ert-deftest lazy-hilit-test ()
  "Test lazy fontification."
  (let ((completion-lazy-hilit t) completion-lazy-hilit-fn)
    (should (equal-including-properties (hotfuzz-all-completions "x" '("x")) '("x")))
    (should (equal-including-properties (funcall completion-lazy-hilit-fn "x")
                                        #("x" 0 1 (face completions-common-part))))))

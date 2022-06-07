;;; tests.el --- The hotfuzz test suite  -*- lexical-binding: t; -*-

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
  (should (< (hotfuzz--cost "ab" "xaxbxx")
             (hotfuzz--cost "ab" "xaxxbx"))))

;;; Highlighting tests

(ert-deftest highlight-optimal-test ()
  "Test that the algorithm is non-greedy."
  (should (ert-equal-including-properties
           (hotfuzz-highlight "ab" "xaxbxabxaxbx")
           #("xaxbxabxaxbx" 5 7 (face completions-common-part)))))

;;; Filtering tests

(ert-deftest case-sensitivity-test ()
  (let ((xs '("aa" "aA" "Aa" "AA")))
    (let ((completion-ignore-case nil))
      (should (equal (hotfuzz-filter "a" xs) '("aa" "aA" "Aa")))
      (should (equal (hotfuzz-filter "A" xs) '("Aa" "AA" "aA"))))
    (let ((completion-ignore-case t))
      (should (equal (hotfuzz-filter "a" xs) xs))
      (should (equal (hotfuzz-filter "A" xs) xs)))))

(ert-deftest all-completions-test ()
  (let* ((completion-styles '(hotfuzz))
         (s "fb")
         (table '("foobar" "fxxx" "foo-baz" "" "fb"))
         (meta (completion-metadata s table nil))
         (candidates (completion-all-completions s table nil (length s) meta))
         (sortfun (alist-get 'display-sort-function meta))
         (last (last candidates)))
    (when (numberp (cdr last)) (setcdr last nil))
    (when sortfun (setq candidates (funcall sortfun candidates)))
    (should (equal candidates '("fb" "foo-baz" "foobar")))))

;; The built-in `flex' completion style fails this test since it
;; allows the search term "s" to match inside of the prefix "/usr/",
;; meaning no completions get filtered.
(ert-deftest boundaries-test ()
  "Test completion on a single field of a filename."
  (let ((completion-styles '(hotfuzz)))
    (should
     (equal
      (completion-all-completions
       "/usr/s/man"
       (lambda (string _pred action)
         (pcase action
           ('metadata '(metadata (category . file)))
           (`(boundaries . ,suffix)
            `(boundaries ,(length (file-name-directory string))
                         . ,(string-match-p "/" suffix)))
           ('t (list "/usr/bin" "/usr/share" "/usr/local"))))
       nil
       6) ; Point as in "/usr/s|/man"
      '("/usr/share" . 5)))))

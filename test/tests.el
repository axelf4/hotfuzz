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

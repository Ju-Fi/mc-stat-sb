(defpackage statsb/tests/main
  (:use :cl
        :statsb
        :rove))
(in-package :statsb/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :statsb)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

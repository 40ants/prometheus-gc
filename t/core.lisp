(uiop:define-package #:prometheus-gc-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:prometheus-gc-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))

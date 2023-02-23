(defsystem "prometheus-gc-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides tests for prometheus-gc."
  :source-control (:git "https://github.com/40ants/prometheus-gc")
  :bug-tracker "https://github.com/40ants/prometheus-gc/issues"
  :pathname "t"
  :depends-on ("prometheus-gc-tests/core")
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c))  )

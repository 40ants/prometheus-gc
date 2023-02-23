(defsystem "prometheus-gc"
  :description "This is a Prometheus collector for Common Lisp implementation garbage collector."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/prometheus-gc"
  :source-control (:git "https://github.com/40ants/prometheus-gc")
  :bug-tracker "https://github.com/40ants/prometheus-gc/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("prometheus-gc/sbcl")
  :in-order-to ((test-op (test-op "prometheus-gc-tests"))))

(defsystem "prometheus-gc-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides documentation for prometheus-gc."
  :source-control (:git "https://github.com/40ants/prometheus-gc")
  :bug-tracker "https://github.com/40ants/prometheus-gc/issues"
  :pathname "docs"
  :depends-on ("prometheus-gc"
               "prometheus-gc-docs/index"))

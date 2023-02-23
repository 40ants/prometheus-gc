(defsystem "prometheus-gc-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides CI settings for prometheus-gc."
  :source-control (:git "https://github.com/40ants/prometheus-gc")
  :bug-tracker "https://github.com/40ants/prometheus-gc/issues"
  :pathname "src"
  :depends-on ("prometheus-gc-ci/ci"))

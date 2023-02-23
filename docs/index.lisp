(uiop:define-package #:prometheus-gc-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:prometheus-gc-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:prometheus-gc-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "prometheus-gc-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (ql:quickload "40ants-doc-theme-40ants")
  #-quicklisp
  (asdf:load-system "40ants-doc-theme-40ants")
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS")))
  )


(defsection @index (:title "prometheus-gc - This is a Prometheus collector for Common Lisp implementation garbage collector."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "TODO"
                                   "Unlicense"
                                   "REPL"
                                   "GIT"))
  (prometheus-gc system)
  "
[![](https://github-actions.40ants.com/40ants/prometheus-gc/matrix.svg?only=ci.run-tests)](https://github.com/40ants/prometheus-gc/actions)

![Quicklisp](http://quickdocs.org/badge/prometheus-gc.svg)
"
  (@about section)
  (@installation section)
  (@usage section))


(defsection-copy @readme @index)


(defsection @about (:title "What is this?")
  """
This is a Prometheus collector for Common Lisp implementation garbage collector.

Only SBCL is currently supported.
""")


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :prometheus-gc)
```
""")


(defsection @usage (:title "Usage"
                    :ignore-words ("ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"))
  """
Instantiate collector like this:

```lisp
(prometheus-gc:make-gc-collector :registry registry)
```

Then serialize metrics as described in the docs of [prometheus.cl](https://github.com/deadtrickster/prometheus.cl) library.

""")

What is this?
=============

This is a Prometheus collector for Common Lisp implementation garbage collector.

Only SBCL is currently supported.

Installation
============

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :prometheus-gc)
```

How to use it?
==============

Instantiate collector like this:

```lisp
(prometheus-gc:make-gc-collector :registry registry)
```

Then serialize metrics as described in the docs of [prometheus.cl](https://github.com/deadtrickster/prometheus.cl) library.

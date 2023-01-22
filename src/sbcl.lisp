(uiop:define-package #:prometheus-gc
  (:use #:cl)
  (:nicknames #:prometheus-gc/sbcl)
  (:import-from #:prometheus)
  (:export
   #:make-gc-collector))
(in-package #:prometheus-gc)


(defclass gc-collector (prometheus:collector)
  ((run-time)
   (bytes-consed)
   #+gencgc
   (generation-average-age)
   #+gencgc
   (generation-bytes-allocated)
   #+gencgc
   (generation-number-of-gcs)

   ;; This should be available for all types of GC
   (setting-bytes-consed-between-gcs)
   
   #+gencgc
   (setting-generation-bytes-consed-between-gcs)
   #+gencgc
   (setting-generation-minimum-age-before-gc)
   #+gencgc
   (setting-generation-number-of-gcs-before-promotion)))


(defun to-label (symbol)
  (substitute #\_ #\- (string-downcase symbol)
              :test #'char=))


(defmethod initialize-instance :after ((mc gc-collector) &rest args)
  (declare (ignore args))

  (setf (slot-value mc 'run-time)
        (prom:make-gauge :name (prom:collector-metric-name mc "sbcl_gc_run_time")
                         :help "SBCL GC Run Time in microseconds."
                         :value 0
                         :registry nil))
  (setf (slot-value mc 'bytes-consed)
        (prom:make-gauge :name (prom:collector-metric-name mc "sbcl_gc_bytes_consed")
                         :help "The number of bytes consed since the program began."
                         :value 0
                         :registry nil))
  
  (let ((gauges
          '(#+gencgc
            (generation-average-age "Average age of memory allocated to generation.")
            #+gencgc
            (generation-bytes-allocated "Number of bytes allocated to generation currently.")
            #+gencgc
            (generation-number-of-gcs "Number of times garbage collection has been done on generation without promotion.")

            ;; This should be available for all types of GC
            (setting-bytes-consed-between-gcs
             "The amount of memory that will be allocated before the next garbage collection is initiated.")
            #+gencgc
            (setting-generation-bytes-consed-between-gcs
             "Number of bytes that can be allocated to generation before that generation is considered for garbage collection.")
            #+gencgc
            (setting-generation-minimum-age-before-gc
             "Minimum average age of objects allocated to generation before that generation is may be garbage collected.")
            #+gencgc
            (setting-generation-number-of-gcs-before-promotion
             "Number of times garbage collection is done on generation before automatic promotion to the next generation is triggered."))))

    (loop for (slot-name help) in gauges
          for metric-name = (format nil "sbcl_gc_~A"
                                    (to-label slot-name))
          for labels = (when (search "generation" metric-name :test #'string-equal)
                         (list "generation"))
          do (setf (slot-value mc slot-name)
                   (prom:make-gauge :name (prom:collector-metric-name mc metric-name)
                                    :help help
                                    :labels labels
                                    :registry nil)))))


(defun make-gc-collector (&key (namespace "") (name "sbcl_gc_collector") (registry prom:*default-registry*))
  (let ((collector (make-instance 'gc-collector
                                  :namespace namespace
                                  :name name)))
    (when registry
      (prom:register collector registry))
    collector))


(defmethod prometheus:collect ((mc gc-collector) cb)
  (labels ((send (gauge)
             (funcall cb gauge))
           (%set (gauge value &key labels)
             (prom:gauge.set gauge
                             value
                             :labels labels))
           (set&send (gauge value &key labels)
             (%set gauge value :labels labels)
             (send gauge)))
    (with-slots (run-time
                 bytes-consed
                 setting-bytes-consed-between-gcs)
        mc
      (set&send run-time sb-ext:*gc-run-time*)
      (set&send bytes-consed (sb-ext:get-bytes-consed))
      (set&send setting-bytes-consed-between-gcs (sb-ext:bytes-consed-between-gcs)))
  
    #+gencgc
    (with-slots (generation-average-age
                 generation-bytes-allocated
                 generation-number-of-gcs
                 setting-generation-bytes-consed-between-gcs
                 setting-generation-minimum-age-before-gc
                 setting-generation-number-of-gcs-before-promotion)
        mc
      (loop for gen upto 6
            for labels = (list (format nil "~A" gen))
            do (%set generation-average-age
                     (sb-ext:generation-average-age gen)
                     :labels labels)
               (%set generation-bytes-allocated
                     (sb-ext:generation-bytes-allocated gen)
                     :labels labels)
               (%set generation-number-of-gcs
                     (sb-ext:generation-number-of-gcs gen)
                     :labels labels)
               (%set setting-generation-bytes-consed-between-gcs
                     (sb-ext:generation-bytes-consed-between-gcs gen)
                     :labels labels)
               (%set setting-generation-minimum-age-before-gc
                     (sb-ext:generation-minimum-age-before-gc gen)
                     :labels labels)
               (%set setting-generation-number-of-gcs-before-promotion
                     (sb-ext:generation-number-of-gcs-before-promotion gen)
                     :labels labels))
      (mapc #'send
            (list generation-average-age
                  generation-bytes-allocated
                  generation-number-of-gcs
                  setting-generation-bytes-consed-between-gcs
                  setting-generation-minimum-age-before-gc
                  setting-generation-number-of-gcs-before-promotion)))

    (values)))


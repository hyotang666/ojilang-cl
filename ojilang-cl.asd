; vim: ft=lisp et
(in-package :asdf)
(defsystem "ojilang-cl"
  :version "0.11.6"
  :depends-on
  nil
  :pathname
  "src/"
  :components
  ((:file "ojilang-cl")))

;; These two methods below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "ojilang-cl"))))
  (append (call-next-method) '((test-op "ojilang-cl.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "ojilang-cl")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))

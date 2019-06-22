; vim: ft=lisp et
(in-package :asdf)
(defsystem :ojilang-cl.test
  :depends-on
  (:jingoh "ojilang-cl")
  :components
  ((:file "ojilang-cl"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :ojilang-cl args)))
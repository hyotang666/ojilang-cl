(in-package :cl-user)
(defpackage :ojilang-cl
  (:use :cl)
  (:export))
(in-package :ojilang-cl)

; quote block progn catch locally go progv symbol-macrolet load-time-value macrolet unwind-protect flet throw eval-when multiple-value-call if return-from let the tagbody setq let* labels multiple-value-prog1 function

(defvar *vocabulary* (make-hash-table))

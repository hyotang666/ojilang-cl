(in-package :cl-user)
(defpackage :ojilang-cl
  (:use :cl)
  (:export))
(in-package :ojilang-cl)

; quote block progn catch locally go progv symbol-macrolet load-time-value macrolet unwind-protect flet throw eval-when multiple-value-call if return-from let the tagbody setq let* labels multiple-value-prog1 function

(defvar *vocabulary* (make-hash-table))

(defmacro defoji(symbol lambda-list &body body)
  `(setf (gethash ',symbol *vocabulary*)
	 (lambda,lambda-list,@body)))

(defun ojilang(expression)
  (typecase expression
    (atom expression)
    (list (let((lang(gethash(car expression)*vocabulary*)))
	    (if lang
	      (apply lang (cdr expression))
	      (default-funcall expression))))))

(defun default-funcall(expression)
  (format nil "僕と~Sどうかな😜❓~{~A❓~}ナンチャッテ（笑）"
	  (car expression)
	  (mapcar #'ojilang (cdr expression))))

(defoji quote(expression)
  (format nil "~Sとかどうかな😅😅❓" expression))

(defoji function(expression)
  (format nil "~Sとかする🎵😆❓" expression))

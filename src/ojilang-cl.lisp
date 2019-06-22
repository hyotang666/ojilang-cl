(in-package :cl-user)
(defpackage :ojilang-cl
  (:use :cl)
  (:export))
(in-package :ojilang-cl)

;;;; IMPLEMENTED
; quote function let let* if

;;;; NIY
; block progn catch locally go progv symbol-macrolet load-time-value macrolet unwind-protect flet throw eval-when multiple-value-call return-from the tagbody setq labels multiple-value-prog1

(defvar *vocabulary* (make-hash-table))

(defmacro defoji(symbol lambda-list &body body)
  `(setf (gethash ',symbol *vocabulary*)
	 (lambda,lambda-list,@body)))

(defun ojilang(expression)
  (typecase expression
    (atom (prin1-to-string expression))
    (list (let((lang(gethash(car expression)*vocabulary*)))
	    (if lang
	      (apply lang (cdr expression))
	      (if(macro-function(car expression))
		(ojilang(macroexpand expression))
		(default-funcall expression)))))))

(defun open-paren()
  (format nil "ヤッホー(^з<)"))

(defun close-paren()
  (format nil "(^_^)v"))

(defun default-funcall(expression)
  (format nil "~Sとか好き😍😍❓~{~Aとか❓~}（≧ ∇ ≦ ）"
	  (car expression)
	  (mapcar #'ojilang (cdr expression))))

(defoji quote(expression)
  (format nil "~Sとかどうかな😅😅❓" expression))

(defoji function(expression)
  (format nil "~Sとかする🎵😆❓" expression))

(defoji let(binds &rest body)
  (format nil "~A🎵~%~:{~Sチャン、~@[~A~]~%~}ﾁｭｯ😘❤️ ❤️ ~%~{~A~%~}~A"
	  (open-paren)
	  (mapcar (lambda(bind)
		    (etypecase bind
		      (atom (list bind nil))
		      ((cons * null)(list(car bind)nil))
		      ((cons * (cons * null))
		       (list (car bind)(ojilang(cadr bind))))))
		  binds)
	  (mapcar #'ojilang body)
	  (close-paren)))

(defoji let*(binds &rest body)
  (format nil "~A🎶~%~:{~Sチャン、~@[~A~]~%~}ﾁｭｯ😘❤️ ❤️ ~%~{~A~%~}~A"
	  (open-paren)
	  (mapcar (lambda(bind)
		    (etypecase bind
		      (atom (list bind nil))
		      ((cons * null)(list(car bind)nil))
		      ((cons * (cons * null))
		       (list (car bind)(ojilang(cadr bind))))))
		  binds)
	  (mapcar #'ojilang body)
	  (close-paren)))

(defoji if(pred then &optional else)
  (format nil "もしかしてだけど、~Aﾉｼ~%だとしたら~A❤️~%~@[えっ❓違う❓❓💦💦~%じゃあ~A~]ナンチャッテ（笑）"
	  (ojilang pred)
	  (ojilang then)
	  (ojilang else)))

(defoji tagbody(&rest args)
  (format nil "最近連絡くれないけど、忙しいのかな❓~%~{~A~%~}今日も素敵な夢が見られますように❤️ "
	  (mapcar (lambda(arg)
		    (typecase arg
		      (atom (format nil "~Sとか好きだったよね❓" arg))
		      (t (ojilang arg))))
		  args)))

(defoji go(tag)
  (format nil "そろそろおじさんと~Sとか行こうよ😝" tag))

(defoji block(tag &rest args)
  (format nil "おじさんも~Sに行くの、すっごくスキ（＞＿＜）なんだよ❗~%~{~A~%~}なんだか、ドキドキ❤️ しちゃうね(^_^)"
	  tag
	  (mapcar #'ojilang args)))

(defoji return-from(tag &optional return)
  (format nil "今度一緒に~S行こうよ❗~@[~%~Aだよ❗❓~]~%楽しみだね🥰 🥰 "
	  tag
	  (ojilang return)))

(defoji loop(&rest args)
  (format nil "いつまでも一緒にいようね❤️ ❤️ ~%~{~A~%~}大スキだよ💏"
	  (mapcar #'ojilang args)))

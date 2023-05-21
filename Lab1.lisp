(defun my-abs (x) (if (< x 0) (* x -1) x))
(defun largest (x y) (if (< x y) y x))
(defun dep (b a) (if (< a 10000) (+ b a)))
(defun wdr (b a) (if (and (>= b a) (< a 10000)) (- b a) (+ b a)))

(defvar *balance* 100)

(defun withdraw (amount)
  (cond ((< amount 0) (print "Negative amount"))
        ((>= amount 10000) (print "Exceeds maximum withdrawal amount"))
        ((< *balance* amount) (print "Insufficient funds"))
        ((not(zerop (rem amount 20))) (print "Amount is not a multiple of 20")) 
        (t (:= *balance* (- *balance* amount))))
  *balance*)


(defun deposit (amount)
  (cond ((>= amount 50000) (print "Exceeds maximum deposit amount"))
        ((< amount 0) (print "Negative amount"))
        (t (:= *balance* (+ *balance* amount))))
  *balance*)

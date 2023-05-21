(defun a-sum (a b)
  (do ((i a (1+ i))
       (sum1 0 (+ sum1 i)))
      ((> i b) sum1)))


(defun sum-odd (a b)
  (let ((start a))
  (when (evenp a) (:= start (+ a 1)))
  (do ((i start (+ i 2))
       (sum2 0 (+ sum2 i)))
      ((> i b) sum2))))


(defun my-function (f)
  (funcall f 1))


(defun sigma (f n p)
  (do ((i n (+ i 1))
       (sum3 0 (+ sum3 (funcall f i))))
      ((> i p) sum3)))

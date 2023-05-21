(defun arith-eval (expr)
  "EXPR is a list of symbols that may include:
square brackets, arithmetic operations, and numbers."
  (let ((ops ())
        (vals ())
        (op nil)
        (val nil)
        (open-sqr ()))
    (dolist (item expr)
      (case item
        ([ (push item open-sqr))
        ((+ - * / ^ sdiv maxf fact) (push item ops))
        (] (cond ((null open-sqr)
                  (push "Error" vals) (print "Unbalanced number of brackets in the expression") (return))
                 (t
                  (pop open-sqr)
                  (setf op (pop ops) val (pop vals))
                  (case op
                    (+ (setf val (+ val (pop vals))))
                    (- (setf val (- (pop vals)  val)))
                    (* (setf val (* val (pop vals))))
                    (/ (setf val (/ (pop vals)  val)))
                    (^ (setf val (expt (pop vals) val)))
                    (sdiv (let* ((my-y (pop vals)) (my-x (pop vals))) (setf val (/ (- my-x my-y) val))))
                    (maxf (setf val (max (pop vals) (pop vals) val)))
                    (fact (let ((product 1)) (dotimes (i val) (setf product (* product (1+ i)))) (setf val product))))
                  (push val vals))))
        (otherwise (push item vals))))
    (cond ((null open-sqr) (pop vals)) (t (print "Unbalanced number of brackets in the expression") "Error"))))

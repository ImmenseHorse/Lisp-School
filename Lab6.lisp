(defun next-state (a-list key)
  (cdr (assoc key a-list)))

(defun jump-state (std s x)
  (cond ((and (listp (next-state std x)) (member s (next-state std x)) (not (null (next-state std s)))) (next-state std s))
        ((and (equal s (next-state std x)) (not (null (next-state std s)))) (next-state std s))
        (t x)))

(defun next-fork (std x)
  (cond ((null (next-state std x)) nil)
        ((listp (next-state std x)) (next-state std x))
        (t (next-fork std (next-state std x)))))

(defun interleave (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        (t (append (list (car l1) (car l2)) (interleave (cdr l1) (cdr l2))))))

(defun comb (n k)
  (cond ((or (and (= k 0) (>= n 0))
             (and (= n k) (>= n 0))) 1)
        ((and (> n k) (> k 0)) (+ (comb (1- n) k) (comb (1- n) (1- k))))
        (t nil)))

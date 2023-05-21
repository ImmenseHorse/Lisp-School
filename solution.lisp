(defun has-loop (g s)
  (labels
      ((my-helper (g s &optional traversed)
         (when g
           (let ((items (assoc s g)))
             (when items (let ((key (cdr items))
                               (passed (car items))
                               (traversed1 (cons s traversed)))
                           (when key
                             (if (listp key) (or (not (null (member (car key) traversed1)))
                                                 (not (null (member (nth 1 key) traversed1)))
                                                 (my-helper g (car key) (cons passed traversed1))
                                                 (my-helper g (nth 1 key) (cons passed traversed1)))
                                 (or (not (null (member key traversed1))) (my-helper g key (cons passed traversed1)))))))))))
    (my-helper g s)))

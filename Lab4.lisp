(defstruct movie
  title  director year type)

(defparameter *size* 3)

(defvar *db*)

(setf *db* (make-array *size*  :initial-element nil))

(defvar *db-list*)

(setf *db-list*  nil)  

(defun quicksort (vec comp)
  (when (> (length vec) 1)
    (let ((pivot-i 0)
          (pivot (aref vec (1- (length vec)))))
      (dotimes (i (1- (length vec)))
        (when (funcall comp (aref vec i) pivot)
          (rotatef (aref vec i)
                   (aref vec pivot-i))
          (incf pivot-i)))
      (rotatef (aref vec (1- (length vec)))
               (aref vec pivot-i))
      (quicksort (rtl:slice vec 0 pivot-i) comp)
      (quicksort (rtl:slice vec (1+ pivot-i)) comp)))
  (dotimes (i *size*)
    (when (not (null (aref *db* i))) (return *db*))))

(defun add-movie (m)
  "Adds a movie to the DB and returns true"
  (dotimes (i *size*)
    (when (null (aref *db* i))
      (setf (aref *db* i) m)
      (return *db*))))

(defun add-movie-list (m)
  "Adds a movie to the end of *db-list* and returns the list"
  (cond ((in-db-list? (movie-title m)) nil)
        (t (push m *db-list*))))

(defun sort-title ()
  (quicksort *db* (lambda (x y)
                    (if (and (typep x 'movie) (typep y 'movie))
                        (string< (movie-title x) (movie-title y))
                        t))))

(defun sort-year ()
  (quicksort *db* (lambda (x y) (if (and (typep x 'movie) (typep y 'movie))
                                    (< (movie-year x) (movie-year y))
                                    t))))

(defun in-db-list? (title)
  (dolist (i *db-list* nil)
    (if (string= (movie-title i) title) (return *db-list*))))

(defun from-year (year)
  (let ((myL nil))
    (dolist (i *db-list*)
      (if (= (movie-year i) year) (push i myL)))
    myL))

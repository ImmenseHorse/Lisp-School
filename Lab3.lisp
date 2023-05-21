(defstruct movie
  title  director year type)

(defparameter *size* 3) 

(defvar *db*)

(setf *db* (make-array *size*  :initial-element nil))

(defun add-movie (m)
  "Adds a movie to *db* and returns *db*"
  (dotimes (i *size*)
    (when (and (not (in-db? (movie-title m))) (null (aref *db* i)))
      (setf (aref *db* i) m)
      (return *db*))))

(defun in-db? (title)
  "Returns *db* if movie title is in the database; otherwise returns NIL"
  (dotimes (i *size*)
    (when (and (typep (aref *db* i) 'movie)
               (equal (movie-title (aref *db* i)) title))
      (return *db*))))

(defun delete-movie (title)
  (dotimes (b *size*)
    (when (and (typep (aref *db* b) 'movie)
               (equal (movie-title (aref *db* b)) title))
      (dotimes (a (- *size* b))
        (if (= (+ a b) (1- *size*))
            (setf (aref *db* (+ a b)) nil)
            (setf (aref *db* (+ a b)) (aref *db* (1+ (+ a b))))))
      (return *db*))))

(defun replace-movie (m nm)
  (dotimes (i *size*)
    (when (and (in-db? (movie-title m)) (not (in-db? (movie-title nm)))
               (equal (movie-title m) (movie-title (aref *db* i))))
      (setf (aref *db* i) nm)
      (return t))))

(defun num-movies ()
  (let ((total 0))
    (dotimes (i *size* total)
      (when (not (null (aref *db* i))) (:= total (1+ total))))))

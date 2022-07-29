(load "../common.lisp")

(defun toggle (n)
  "Toggles value n"
  (if (= n 0) 1 0))

(defun parse-coordinates (query)
  "Parses coordinates (x,y through a,b) and zips them ((x a) (y b))"
    (mapcar #'list
    (mapcar 'parse-integer (split-by-delimeter (first query) #\,))
    (mapcar 'parse-integer (split-by-delimeter (car (last query)) #\,))))

(defun a (b)
    (let ((vals (parse-coordinates b)))
      (let ((cols (pop vals)))
      (let ((rows (pop vals)))
        (loop for x from (pop cols) to (pop cols)
              nconcing (loop for y from (first rows) to (car (last rows))
                             collect (list x y)))))))
                       ;; do (format t "~a ~a~%" x y))))))))

(defun apply-operator (coordinates grid)
  (if (equal (pop coordinates) "toggle")
    (loop for coordinate in (a coordinates)
          do (let ((val (aref grid (first coordinate) (car (last coordinate)))))
               (setf (aref grid (first coordinate) (car (last coordinate))) (toggle val))))
    (if (equal (pop coordinates) "on")
      (loop for coordinate in (a coordinates)
            do (let ((val (aref grid (first coordinate) (car (last coordinate)))))
                 (setf (aref grid (first coordinate) (car (last coordinate))) 1)))
      (loop for coordinate in (a coordinates)
            do (let ((val (aref grid (first coordinate) (car (last coordinate)))))
                 (setf (aref grid (first coordinate) (car (last coordinate))) 0))))))

(defun set-value (arr x y n)
  (setf (aref arr x y) n))

(defun main ()
  (let ((strings
          (loop for str = (read-line *standard-input* nil nil)
                while str
                collect (split-by-delimeter (string str) #\Space))))
  (let ((grid (make-array (list 1000 1000))))
    (loop for x in strings
          do (apply-operator x grid))
    (print (reduce #'+(loop for x across grid
          nconcing x))))))
    ;; (print (a (pop strings)))))
    ;; (write (a ))))
    ;;(split-by-delimeter (string str) #\Space))))

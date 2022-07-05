;; Basically stolen from https://lispcookbook.github.io/cl-cookbook/strings.html
(defun split-by-x (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\X string :start i)
          collect (subseq string i j)
          while j))

(defun smallest (list)
  (apply 'min list))

(defun rotate (l)
  (append (cdr l) (cons (car l) ())))

(defun gift-area (dimensions)
  (let ((sides (mapcar #'* dimensions (rotate dimensions))))
    (reduce #'+ (append (mapcar (lambda(i) (* 2 i)) sides) (cons (smallest sides) ())))))

(defun main ()
  (let ((gifts
          (remove nil (loop for gift = (read *standard-input* nil nil)
            collect (if gift (mapcar 'parse-integer (split-by-x (string gift))))
            while gift))))
    (write (reduce #'+ (mapcar 'gift-area gifts)))))

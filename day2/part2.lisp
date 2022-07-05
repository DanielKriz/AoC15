;; Basically stolen from https://lispcookbook.github.io/cl-cookbook/strings.html
(defun split-by-x (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\X string :start i)
          collect (subseq string i j)
          while j))

(defun ribbon-len (dimensions)
  (let ((dimensions (sort dimensions #'<)))
    (reduce #'+
      (append
        (list
          (reduce #'* dimensions))
          (mapcar (lambda(i) (* 2 i)) (list(pop dimensions) (pop dimensions)))))))

(defun main ()
  (let ((gifts
          (remove nil (loop for gift = (read *standard-input* nil nil)
            collect (if gift (mapcar 'parse-integer (split-by-x (string gift))))
            while gift))))
    (write (reduce #'+ (mapcar 'ribbon-len gifts)))))

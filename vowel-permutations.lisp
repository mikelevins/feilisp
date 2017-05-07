(defparameter $base-vowels '("a" "e" "i" "o" "u"))

(defun permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (permutations (remove element list)))))))

(defmethod permutations->csv ((path pathname))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for p in (permutations $base-vowels)
       do (format out "~{~A~^, ~}~%" p))))

(defmethod permutations->csv ((path string))
  (permutations->csv (pathname path)))

;;; (permutations->csv "/Users/mikel/Desktop/vowel-permutations.csv")

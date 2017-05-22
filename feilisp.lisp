;;;; feilisp.lisp

(in-package #:feilisp)

(defparameter $consonants
  '(m n g f d s x c h r l i u))

(defmethod any ((s sequence))
  (elt s (random (length s))))

(defun digraphs ()
  (mapcar (lambda (pair)
            (intern (concatenate 'string
                                 (symbol-name (first pair))
                                 (symbol-name (second pair)))
                    :feilisp))
          (remove-duplicates
           (reduce #'append
                   (loop for left in $consonants
                      collect (loop for right in $consonants
                                 collect (list left right)))))))

(defparameter $digraphs (digraphs))

(defmethod list-difference ((l null) r) nil)
(defmethod list-difference (l (r null)) l)
(defmethod list-difference ((l null)(r null)) nil)

(defmethod list-difference ((l cons)(r cons))
  (let ((result l))
    (loop for thing in r
       do (setf result (remove thing result :test #'equal)))
    result))

(defparameter $initials
  '(m n g f d s x c h r l i u
    mn mi mu
    ni nu
    gi gu
    fh fr fl fi fu
    dh dr dl di du
    sm sn sf sd sc sh sr sl si su
    xm xn xf xd xc xh xr xl xi xu
    cm cn ch cr cl ci cu
    hr hl hi hu
    rh ri ru
    lh li lu))

(defparameter $finals
  '(m n g f d s x c h r l i u
    mn mf md ms mx mc
    nf nd ns nx nc
    gs gx gc
    fs fx
    ds dx
    cs
    hm hn hg hf hd hs hx hc hr hl
    rm rn rg rf rd rs rx rc rl
    lm ln lg lf ld ls lx lc lh
    im in ig if id is ix ic ir il
    um un ug uf ud us ux uc))


(defparameter $initials-in-use
  '(c cl cr cu d di dl dr du f fi fl fr fu g h hl hr hu i l li lu m mi mu n ni nu r s sc sd sf sl sm sn su u x xl xr xu))

(defparameter $finals-in-use
  '(c cs d f g gc h i ic id il in ir is ix l ld ls m n nd ns nx r rc
    rd rf rs s u ud uf um un x))

(defparameter $medial-clusters
  (remove-duplicates
   (reduce #'append
           (loop for f in $finals-in-use
              collect (loop for i in $initials-in-use
                         collect (list f i))))
   :test #'equal))

(defparameter $vowels
  '(i u o e a ii uu oo ee aa))

(defparameter $diphthongs
  '(iu uo oe ea ai ia ae eo ou ui))

(defparameter $cv-particles
  (remove-duplicates
   (reduce #'append
           (loop for left in $initials
              collect (loop for right in (append $vowels $diphthongs)
                         collect (list left right))))))

(defparameter $vc-particles
  (remove-duplicates
   (reduce #'append
           (loop for left in (append $vowels $diphthongs)
              collect (loop for right in $finals
                         collect (list left right))))))

(defparameter $cvc-syllables
  (remove-duplicates
   (reduce #'append
           (loop for left in $initials
              collect (loop for right in $finals
                         collect (list left right))))))

(defun print-syllable (syl)
  (let ((*print-case* :downcase))
    (format t "~A-~A" (first syl)(second syl))))

(defun print-syllables (syls)
  (let ((*print-case* :downcase))
    (format t "~%")
    (loop for s in syls
       do (format t "~A-~A~%" (first s)(second s)))))

(defun any-n-syllables (n)
  (let ((result nil))
    (loop while (> n (length result))
       do (pushnew (any $syllables) result))
    result))

(defmethod save-cvc-syllables ((path pathname))
  (let ((*print-case* :downcase))
    (with-open-file (out path :direction :output)
      (loop for syllable in $cvc-syllables
         do (format out "~A-~A~%"
                    (first syllable)
                    (second syllable))))))

(defmethod save-cvc-syllables ((path string))
  (save-cvc-syllables (pathname path)))

;;; (save-cvc-syllables "/Users/mikel/Desktop/fey-syllables.txt")

(defmethod save-base-vocabulary ((path pathname))
  (let* ((heads (remove-duplicates (mapcar #'first $syllables))))
    (let ((*print-case* :downcase))
      (with-open-file (out path :direction :output)
        (let* ((syllables-table (loop for head in heads
                                   collect (cons head
                                                 (remove-if-not (lambda (x)(eql head (car x)))
                                                                $syllables))))
               (col-count (length syllables-table))
               (row-count (apply #'max (mapcar #'length syllables-table)))
               (syllables-array (make-array (list col-count row-count) :initial-contents syllables-table)))
          (loop for y from 0 below row-count
             do (progn
                  (loop for x from 0 below col-count
                     do (let ((it (aref syllables-array x y)))
                          (typecase it
                            (symbol (format out "~A, " it))
                            (cons (format out "~A-~A, " (first it)(second it)))
                            (t (error "Invalid element: ~S" it)))))
                  (format out "~%"))))))))

;;; (save-base-vocabulary "/Users/mikel/Desktop/fey-base-vocab.txt")

(defmethod save-base-vocabulary ((path string))
  (save-base-vocabulary (pathname path)))

(defmethod save-medial-clusters ((path pathname))
  (let ((*print-case* :downcase))
    (with-open-file (out path :direction :output)
      (loop for cluster in $medial-clusters
         do (format out "~A,~A,~%"
                    (first cluster)
                    (second cluster))))))

(defmethod save-medial-clusters ((path string))
  (save-medial-clusters (pathname path)))

;;; (save-medial-clusters "/Users/mikel/Desktop/fey-medials.csv")

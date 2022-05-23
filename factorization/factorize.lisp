(defun n-to-1 (x)
  "Generates a list from [n] to 1, with a step size of -1."
  (if (= x 0) '() (cons x (n-to-1 (- x 1)))))

(defun make-1-to-n-list (n)
  "Generates a list from 1 to [n] with a step size of 1."
  (reverse (n-to-1 n)))
  
(defun divides (n m)
  "Does [n] divide [m]?"
  (= (mod m n) 0))

(defun factorize (n)
  "Factorize takes a number [n] and returns a list of all of it's factors."
  (remove-if (lambda (x) (not (divides x n))) (make-1-to-n-list n)))


; Credit:
; http://dnaeon.github.io/generating-sequences-in-common-lisp/
(defun iota-generator (&key (start 0) (step 1))
  (let ((i 0))
    (lambda ()
      (prog1 start (incf start step) (incf i)))))


(defparameter *iota* (iota-generator))

; Look into using a generator...
(defun main ()
  (loop
    (print (factorize (funcall *iota*)))))

;(main)

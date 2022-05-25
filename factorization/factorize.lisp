;; TODO - https://lispcookbook.github.io/cl-cookbook/process.html

;; http://cl-cookbook.sourceforge.net/loop.html
(defun factorize (n)
  "factorize takes a number [n] and returns a list containing all of its factors."
  (loop for x from 1 to n
        when (= (mod n x) 0) ;; When x cleanly divides n, collect it.
        collect x))

(defun factorize-iota (start inc_size)
  (print (factorize start))
  (factorize-iota (+ start inc_size) inc_size))

;; TODO - Spin up multiple threads, using a similar technique to the
;; haskell version of this program.
(defun main ()
  (factorize-iota 1 1))

(main)

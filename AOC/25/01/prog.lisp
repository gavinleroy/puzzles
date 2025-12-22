(asdf:load-system :cl-ppcre)

(defpackage :day01
  (:use :cl :cl-ppcre))

(in-package :day01)

(defun parse (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (multiple-value-bind (_ registers)
                      (ppcre:scan-to-strings "^([LR])([0-9]+)$" line)
                    (let ((n (parse-integer (aref registers 1))))
                      (case (intern (aref registers 0))
                        (R n) (L (- n))))))))

(defun solve (path)
  (loop with dial = 50
        for n in (parse path)
        sum (loop repeat (abs n)
                  do (setf dial (mod (+ dial (/ n (abs n))) 100))
                  count (zerop dial)) into pass-count
        count (zerop dial) into zero-count
        finally (return (values zero-count pass-count)))) 

(defun run ()
  (multiple-value-bind (p1 p2) (solve "./1.txt")
    (format t "Part 1: ~a~%Part 2: ~a~%" p1 p2))) 

(eval-when (:execute) (run))

(defpackage :day03
  (:use :cl))

(in-package :day03)

(defun parse (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun max-numbern (s n &key (start 0))
  (when (zerop n)
    (return-from max-numbern 0))
  (loop with digit-idx = (length s) with end = (- (length s) n)
        for i from end downto start
        for c = (aref s i)
        for d = (- (char-int c) 48)
        maximize d into digit 
        when (= d digit)
          do (setf digit-idx i)
        finally (return (+ (* digit (expt 10 (1- n))) 
                           (max-numbern s (1- n) :start (1+ digit-idx)))))) 
           
(defun solve (path) 
  (let* ((input (parse path))
         (p1 (loop for bank in input 
              sum (max-numbern bank 2)))
         (p2 (loop for bank in input 
                   sum (max-numbern bank 12)))) 
    (values p1 p2)))

(defun run () 
  (multiple-value-bind (p1 p2) (solve "./3.txt")
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2))) 

(eval-when (:execute) (run))

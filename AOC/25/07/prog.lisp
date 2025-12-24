(defpackage :day07
  (:use :cl))

(in-package :day07)

(defun solve (path)
  (with-open-file (stream path :direction :input)
    (let* ((first-row (read-line stream))
           (board (make-array (length first-row) :initial-element 0 :element-type '(unsigned-byte 64))))
      (incf (aref board (position #\S first-row)))
      (loop for line = (read-line stream nil nil)
            while line
            sum (loop for c across line
                      for i from 0
                      when (char= c #\^)
                        count (plusp (aref board i)) into splits
                        and do (incf (aref board (1- i)) (aref board i))
                        and do (incf (aref board (1+ i)) (aref board i))
                        and do (setf (aref board i) 0)
                      finally (return splits)) into part-1
            finally (return (values part-1 (reduce #'+ board)))))))


(defun run () 
  (multiple-value-bind (p1 p2) 
      (solve "./7.txt") 
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2)))
     
(eval-when (:execute) (run))

(require :asdf)
(asdf:load-system :str)

(defpackage :day02
  (:use :cl))

(in-package :day02)

(defun parse (path) 
  (mapcar (lambda (s) (mapcar #'parse-integer (str:split "-" s))) 
          (str:split "," (uiop:read-file-string path))))

(defun invalidp (str)
  (loop with n = (length str)
        for period from 1 to (floor n 2)
        thereis (and (zerop (mod n period))
                     (loop for i from period below n by period 
                           always (string= str str 
                                           :end1 period 
                                           :start2 i
                                           :end2 (+ i period))
                           finally (return period)))))

(defun solve (path) 
  (loop with p1 = 0 with p2 = 0
      for (start stop) in (parse path)
      do (loop for i from start to stop
               for st = (princ-to-string i)
               for period = (invalidp st)
               when period
               do (incf p2 i)
               when (and period (= period (floor (length st) 2)))
               do (incf p1 i)) 
      finally (return (values p1 p2))))

(defun run () 
  (multiple-value-bind (p1 p2) (solve "./2.txt")
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2))) 

(eval-when (:execute) (run))

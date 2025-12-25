(require :asdf)
(asdf:load-system :str)

(defpackage :day05
  (:use :cl))

(in-package :day05)

(defun parse (path)
  (with-open-file (stream path :direction :input)
    (let ((ranges (loop for line = (read-line stream nil nil)
                   while (plusp (length line))
                   collect (mapcar #'parse-integer (str:split "-" line))))
          (ids (loop for line = (read-line stream nil nil)
                     while line 
                     collect (parse-integer line))))
      (values ranges ids))))

(defun compile-pred (ranges)
  (loop for (l h) in ranges
        collect `(<= ,l x ,h) into body
        finally (return (compile nil `(lambda (x) ,(cons 'or body))))))

(defun merge-ranges (ranges) 
  (setf ranges (sort ranges #'< :key #'car))
  (loop with start = (caar ranges) and end = (cadar ranges)
        for (ss ee) in (cdr ranges)
        if (<= ss (1+ end))
          do (setf end (max end ee))
        else
          collect (list start end) into merged
          and do (setf start ss end ee)
        finally (return (cons (list start end) merged))))

(defun solve (path)
  (multiple-value-bind (ranges ids) (parse path)
    (let* ((merged (merge-ranges ranges))
           (p1 (loop with inrangep = (compile-pred merged)
                     for id in ids 
                     count (funcall inrangep id)))
           (p2 (loop for (s e) in merged
                     sum (1+ (- e s))))) 
      (values p1 p2))))

(defun run () 
  (multiple-value-bind (p1 p2) (solve "./5.txt")
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2))) 

(eval-when (:execute) (run))

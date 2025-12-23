(defpackage :day06
  (:use :cl))

(in-package :day06)

(defun parse (path) 
  (with-open-file (stream path :direction :input)
   (loop for line = (read-line stream nil nil)
         while line
         collect line)))

(defun part-1 (lines) 
  (loop for line in (reverse lines)
        collect (read-from-string (concatenate 'string "(" line ")")) into matrix
        finally (return (eval (cons '+ (apply #'mapcar #'list matrix))))))

(defun part-2 (lines)
  (let ((m (length (car lines)))
        (rlines (nreverse lines)))
    (loop with nums 
          for j from (1- m) downto 0
          for op = (aref (car rlines) j)
          do (push (loop for line in (cdr rlines)
                         for c = (aref line j)
                         unless (char= c #\Space)
                           collect c into num
                         finally (return (parse-integer (coerce (nreverse num) 'string)))) 
                   nums)
          unless (char= op #\Space) 
            sum (eval (cons (intern (string op)) nums)) into ans
            and do (decf j)
            and do (setf nums nil)
          finally (return ans))))

(defun solve (path)
  (let ((lines (parse path)))
    (values (part-1 lines) (part-2 lines))))
  
(defun run () 
  (multiple-value-bind (p1 p2) 
      (solve "./6.txt") 
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2)))
     
(eval-when (:execute) (run))

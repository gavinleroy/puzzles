(defpackage :day04
  (:use :cl))

(in-package :day04)

(defun parse (path) 
  (let* ((ls (uiop:read-file-lines path)
          (n (length ls)) 
          (m (length (car ls))))) 
    (make-array (list n m)
                :element-type 'character 
                :initial-contents (mapcar (lambda (s) (coerce s 'list))
                                          ls))))
  
(defconstant +dirs+ 
  (loop for i from -1 to 1 
        nconc (loop for j from -1 to 1 
                    unless (and (zerop i) (zerop j))
                    collect (list i j))))

(defun neighbors (i j)
  (loop for (ii jj) in +dirs+
        collect (list (+ ii i) (+ jj j)))) 

(defun removable (board)
  (destructuring-bind (n m) (array-dimensions board) 
    (labels ((inboundsp (dir)
                      (and (<= 0 (car dir))
                           (<= 0 (cadr dir))
                           (< (car dir) n)
                           (< (cadr dir) m))) 
             (b@ (dir) (aref board (car dir) (cadr dir)))
             (paperp (dir) 
               (and (inboundsp dir) 
                    (char= (b@ dir) #\@)))) 
      (loop for i from 0 below n
            nconc (loop for j from 0 below m
                        when (and (paperp (list i j))
                                  (< (loop for dir in (neighbors i j) count (paperp dir)) 4))
                        collect (list i j))))))

(defun solve (path) 
  (let ((board (parse path)))
   (let ((p1 (length (removable board)))
         (p2 (loop for to-remove = (removable board)
                   while to-remove
                   do (loop for (i j) in to-remove 
                            do (setf (aref board i j) #\.))
                   sum (length to-remove))))
     (values p1 p2))))

(defun run () 
  (multiple-value-bind (p1 p2) (solve "./4.txt")
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2))) 

(eval-when (:execute) (run))
             
             
           
    
  
                      
                      

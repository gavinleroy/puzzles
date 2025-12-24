(asdf:load-system :str)

(defpackage :day09
  (:use :cl :str))

(in-package :day09)

(defun parse (path)
  (let ((points (mapcar (lambda (s)
                         (mapcar #'parse-integer (str:split "," s)))
                 (uiop:read-file-lines path))))
    (nconc points (list (car points)))))
  
(defun rect (p1 p2)
  (let ((x1 (first p1)) (y1 (second p1))
        (x2 (first p2)) (y2 (second p2)))
    (list (min x1 x2) 
          (max x1 x2)    
          (min y1 y2)   
          (max y1 y2))))  
            
(defun intersects-p (r1 r2)
  (destructuring-bind ((mnx1 mxx1 mny1 mxy1)
                       (mnx2 mxx2 mny2 mxy2)) 
      (list r1 r2)
    (and (< mnx1 mxx2)
         (> mxx1 mnx2)
         (< mny1 mxy2)
         (> mxy1 mny2))))

(defun check-collisions (p1 p2 points)
  (loop for (a b) on points
        while b
        unless (or (member a (list p1 p2) :test #'equalp)
                   (member b (list p1 p2) :test #'equalp))
        when (intersects-p 
               (rect p1 p2)                 
               (rect a b))
          do (return-from check-collisions nil)
        finally (return t)))

(defun area (p1 p2)
  (let ((dx (abs (- (first p1) (first p2))))
        (dy (abs (- (second p1) (second p2)))))
    (* (1+ dx) (1+ dy))))

(defun solve (path)
  (loop with points = (parse path) 
        for p1 in points
        for (a b) = (loop for p2 in points
                          for a = (area p1 p2)
                          maximize a into part-1
                          when (check-collisions p1 p2 points)
                          maximize a into part-2
                          finally (return (list part-1 part-2)))
        maximize a into part-1
        maximize b into part-2
        finally (return (values part-1 part-2))))

(defun run () 
  (multiple-value-bind (p1 p2) 
      (solve "./9.txt") 
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2)))
     
(eval-when (:execute) (run))

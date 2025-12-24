(asdf:load-system :str)

(defpackage :day08
  (:use :cl :str))

(in-package :day08)

;; === union find ===

(defclass union-find ()
  ((parents :accessor parents :initarg :parents)
   (ranks :accessor ranks :initarg :ranks)
   (size :accessor size :initarg :size)))

(defun union-find (size)
  (let ((parents (make-array size :initial-element 0))
        (ranks (make-array size :initial-element 1)))
    (dotimes (i size)
      (setf (aref parents i) i))
    (make-instance 'union-find :parents parents :ranks ranks :size size)))

(defmethod find-set ((uf union-find) i)
  (let ((parents (parents uf)))
    (if (= (aref parents i) i)
        i
        (setf (aref parents i) (find-set uf (aref parents i))))))

(defmethod union-sets ((uf union-find) i j)
  (let ((root-i (find-set uf i))
        (root-j (find-set uf j)))
    (unless (= root-i root-j)
      (let ((ranks (ranks uf))
            (parents (parents uf)))
        (cond ((< (aref ranks root-i) (aref ranks root-j))
               (incf (aref ranks root-j) (aref ranks root-i))
               (setf (aref parents root-i) root-j
                     (aref ranks root-i) 0))
              (t
               (incf (aref ranks root-i) (aref ranks root-j))
               (setf (aref parents root-j) root-i 
                     (aref ranks root-j) 0))))
      (decf (size uf)))))

;; ==================

(defun parse (path)
  (mapcar (lambda (s) (mapcar #'parse-integer (str:split "," s)))
        (uiop:read-file-lines path)))

(defun 3d-distance (point1 point2)
  (let ((dx (- (first point2)  (first point1)))
        (dy (- (second point2) (second point1)))
        (dz (- (third point2)  (third point1))))
    (+ (* dx dx)
       (* dy dy)
       (* dz dz))))

(defun distances (points)
  (loop with half-len = (ceiling (length points) 2)
        for a in points 
        for i from 0
        nconc (loop for b in points 
                    for j from 0
                    unless (<= i j)
                    collect (list (3d-distance a b) i j a b)) into dists
   finally (return (sort dists #'< :key #'car))))

(defun max-n (seq n)
  (subseq (sort (copy-seq seq) #'>) 0 n))
  
(defun solve (path)
  (let* ((points (parse path))
         (ds (distances points))
         (uf (union-find (length points))))
    (loop for (d i j p1 p2) in ds
          for k from 0
          when (= k 1000)
            sum (reduce #'* (max-n (ranks uf) 3)) into part-1
          do (union-sets uf i j)
          when (= 1 (size uf))
            return (values part-1 (* (first p1) (first p2))))))

(defun run () 
  (multiple-value-bind (p1 p2) 
      (solve "./8.txt") 
    (format t "Part 1: ~A~%Part 2: ~A~%" p1 p2)))
     
(eval-when (:execute) (run))

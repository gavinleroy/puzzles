(require :asdf)
(require :sb-concurrency)
(asdf:load-system :cl-ppcre)
(asdf:load-system :str)
(asdf:load-system :linear-programming)
(asdf:load-system :linear-programming-glpk)

(defpackage :day10
  (:import-from :sb-concurrency 
                #:make-queue #:dequeue 
                #:enqueue #:queue-empty-p)
  (:import-from :linear-programming 
                #:solution-objective-value
                #:parse-linear-problem
                #:solve-problem)
  (:use :cl))

(in-package :day10)

(setf linear-programming:*solver* 'linear-programming-glpk:glpk-solver)

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (v f) (funcall f v))
              rest
              :initial-value (apply fn1 args)))))

(defun parse-line (line)
  (let* ((sanitize (compose (lambda (s) (substitute #\Space #\, s))
                            (lambda (s) (substitute #\" #\[ s))
                            (lambda (s) (substitute #\" #\] s))
                            (lambda (s) (substitute #\( #\{ s))
                            (lambda (s) (substitute #\) #\} s))))
         (chunks (mapcar (lambda (s) (read-from-string (funcall sanitize s))) 
                  (str:split " " line))))
    (list (loop for c across (first chunks)
                if (char= #\# c)
                  collect 1
                else 
                  collect 0) 
          (first (reverse chunks))
          (subseq chunks 1 (1- (length chunks))))))

(defun parse (path)
  (mapcar #'parse-line (uiop:read-file-lines path)))

(defconstant +initial-lights+ #*0000000000000000)
(defconstant +initial-counters+ #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(defun counter (v1 v2) (map 'vector #'+ v1 v2))

(defun make-buttons (buttons initial update)
  (labels ((compile-button (seq)
             (let ((buttons (copy-seq initial)))
               (dolist (b seq)
                 (setf (aref buttons b) 1))
               (lambda (bits) 
                 (funcall update (copy-seq bits) buttons)))))
    (mapcar #'compile-button buttons)))

(defun make-target (tgt initial)
  (loop with lights = (copy-seq initial) 
        for c in tgt
        for i from 0
        do (incf (aref lights i) c)
        finally (return lights)))
  
(defun toggle-distance (target buttons &key update)
  (multiple-value-bind (initial update) 
      (ecase update
        (toggle (values +initial-lights+ #'bit-xor))
        (count (values +initial-counters+ #'counter)))
    (loop with q = (make-queue :initial-contents (list (list 0 initial)))
          with buttons = (make-buttons buttons initial update)
          for (d lights) = (dequeue q)
          while (< d 15)
          do (loop for button in buttons
                   for k from 0
                   for step = (funcall button lights)
                   when (equalp step target)
                     do (return-from toggle-distance (1+ d))
                   do (enqueue (list (1+ d) step) q)))))

(defun solve-counters (targets buttons)
  (let* ((num-buttons (length buttons))
         (vars (loop for i below num-buttons 
                     collect (intern (format nil "B~D" i))))
         (objective `(min (+ ,@vars)))
         (constraints 
          (append
           (loop for target-val in targets
                 for counter-idx from 0
                 collect 
                 `(= (+ ,@(loop for button-vector in buttons
                                for var in vars
                                for coef = (elt button-vector counter-idx)
                                unless (zerop coef)
                                collect `(* ,coef ,var)))
                     ,target-val))
           (list `(integer ,@vars))
           (loop for var in vars collect `(>= ,var 0)))))
    (solution-objective-value 
      (solve-problem 
        (parse-linear-problem 
          objective 
          constraints)))))
      
(defun make-matrix (buttons size)
  (loop for row in buttons
        collect (loop with arr = (make-array size :initial-element 0) 
                      for i in row
                      do (incf (aref arr i))
                      finally (return (coerce arr 'list)))))

(defun counter-distance (target buttons)
  (let ((matrix (make-matrix buttons (length target))))
    (handler-case (solve-counters target matrix)
      ;; NOTE: There's one problem that has completely independent 
      ;; buttons: [.#.#] (0,2) (1,3) {7,6,7,6} which results in a 
      ;; "no constraints" error from GLPK, these can easily be solved with BFS
      (error (condition)
        (declare (ignore condition))
        (toggle-distance (make-target target +initial-counters+) buttons 
                         :update 'count))
      (:no-error (n) n)))) 
       
(defun solve (path)
  (loop for (lights joltage buttons) in (parse path)
        sum (toggle-distance (make-target lights +initial-lights+) buttons 
                             :update 'toggle) into part-1
        sum (counter-distance joltage buttons) into part-2
        finally (return (values part-1 part-2))))
        
(defun run () 
  (multiple-value-bind (p1 p2) 
      (solve "./10.txt") 
    (format t "Part 1: ~A~%Part 2: ~D~%" p1 p2)))
     
;PART 2 *ALLEGEDLY* TOO LOW!?!?!?! 
;(solve "./10.txt")
(eval-when (:execute) (run))


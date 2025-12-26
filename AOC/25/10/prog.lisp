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

(defun make-buttons (buttons)
  (labels ((compile-button (seq)
             (let ((buttons (copy-seq +initial-lights+)))
               (dolist (b seq)
                 (setf (aref buttons b) 1))
               (lambda (bits) 
                 (bit-xor (copy-seq bits) buttons)))))
    (mapcar #'compile-button buttons)))

(defun make-target (tgt initial)
  (loop with lights = (copy-seq initial) 
        for c in tgt
        for i from 0
        do (incf (aref lights i) c)
        finally (return lights)))
  
(defun toggle-distance (target buttons)
  (loop with q = (make-queue :initial-contents (list (list 0 +initial-lights+)))
         with buttons = (make-buttons buttons)
         for (d lights) = (dequeue q)
         while (< d 15)
         do (loop for button in buttons
                  for k from 0
                  for step = (funcall button lights)
                  when (equalp step target)
                    do (return-from toggle-distance (1+ d))
                  do (enqueue (list (1+ d) step) q))))

(defun contains-duplicates-p (ls)
  (setf ls (sort ls #'<))
  (loop for (a b) on ls
        while b 
        thereis (= a b)))

(defun solve-counters (targets buttons)
  ;; HACK: If all buttons are disjoint, we have the solution
  (unless (contains-duplicates-p (apply #'concatenate 'list buttons))
    (return-from solve-counters 
                 (reduce #'+ (mapcar (lambda (b) (elt targets (first b))) 
                                     buttons))))
  (let* ((vars (loop for b in buttons 
                     for i from 0
                     collect (intern (format nil "B~D" i))))
         (objective `(min (+ ,@vars)))
         (eq-constraints (loop for target-val in targets
                               for counter-idx from 0
                               collect `(= (+ ,@(loop for button in buttons
                                                      for var in vars
                                                      when (member counter-idx button)
                                                        collect var)) 
                                         ,target-val)))
         (constraints 
          (append
            eq-constraints
            (loop for var in vars 
                  nconc `((integer ,var) (<= 0 ,var))))))
    (solution-objective-value 
      (solve-problem 
        (parse-linear-problem 
          objective 
          constraints)))))

(defun counter-distance (target buttons)
  (solve-counters target buttons)) 
       
(defun solve (path)
  (loop for (lights joltage buttons) in (parse path)
        sum (toggle-distance (make-target lights +initial-lights+) buttons) into part-1
        sum (counter-distance joltage buttons) into part-2
        finally (return (values part-1 part-2))))
        
(defun run () 
  (multiple-value-bind (p1 p2) 
      (solve "./10.txt") 
    (format t "Part 1: ~A~%Part 2: ~D~%" p1 p2)))
     
;PART 2 *ALLEGEDLY* TOO LOW!?!?!?! 
;(solve "./10.txt")
(eval-when (:execute) (run))


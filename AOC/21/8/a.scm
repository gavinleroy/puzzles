#!/usr/local/bin/guile \
-e main -s
!#

;; Gavin Gray AOC 21

(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define stdin (current-input-port))

(define (lines s)
  (string-split s #\newline))

(define (split-pipe s)
  (string-split s #\|))

(define (split-space s)
  (string-split
   (string-trim-both s) #\space))

(define (get-oputs inp)
  (map split-space
       (flatten (map cdr
            (map split-pipe
                 (lines inp))))))

(define (all)
  (get-string-all stdin))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define (count unqs)
  (length (filter (lambda (s)
                    (let ((l (string-length s)))
                      (or (= l 2)
                          (= l 3)
                          (= l 4)
                          (= l 7)))) unqs)))

(define (main _)
  (display
   (fold (lambda (l acc) (+ acc (count l)))
         0 (get-oputs (all))))
  (newline))

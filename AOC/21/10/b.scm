#!/usr/local/bin/guile \
-e main -s
!#

;; Gavin Gray AOC 21

(use-modules (ice-9 textual-ports)
             (ice-9 common-list)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-11))

(define stdin (current-input-port))
(define-syntax-rule (lines s) (string-split s #\newline))
(define-syntax-rule (is-open c)
  (some (lambda (v)
          (char=? c v)) '(#\( #\{ #\[ #\<)))
(define-syntax-rule (board)
  (map string->list
       (lines (get-string-all stdin))))

(define (val c)
  (match c
   (#\( 1)
   (#\[ 2)
   (#\{ 3)
   (#\< 4)))

(define (score stack)
  (fold (lambda (c acc)
                (+ (* 5 acc) (val c))) 0  stack))

(define (is-match o c)
  (or (and (char=? o #\() (char=? c #\)))
      (and (char=? o #\{) (char=? c #\}))
      (and (char=? o #\[) (char=? c #\]))
      (and (char=? o #\<) (char=? c #\>))))

(define (pop ls stack)
  (cond
   ((is-match (car stack) (car ls))
    (match-line (cdr ls) (cdr stack)))
   (else 0)))

(define (match-line ls stack)
  (cond
   ((and (null? ls)
         (list? stack)) (score stack))
   ((is-open (car ls))
    (match-line (cdr ls) (cons (car ls) stack)))
   (else (pop ls stack))))

(define (solve board)
  (let* ((l (map (lambda (l) (match-line l '())) board))
         (l (filter (lambda (v) (> v 0)) l))
         (len (length l)))
    (list-ref (sort l <)
              (floor-quotient len 2))))

(define (main _)
  (display (solve (board)))
  (newline))

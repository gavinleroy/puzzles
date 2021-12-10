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
   (#\)     3)
   (#\]    57)
   (#\}  1197)
   (#\> 25137)))

(define (is-match o c)
  (or (and (char=? o #\() (char=? c #\)))
      (and (char=? o #\{) (char=? c #\}))
      (and (char=? o #\[) (char=? c #\]))
      (and (char=? o #\<) (char=? c #\>))))

(define (pop ls stack)
  (cond
   ((is-match (car stack) (car ls))
    (match-line (cdr ls) (cdr stack)))
   (else (val (car ls)))))

(define (match-line ls stack)
  (cond
   ((null? ls) 0)
   ((is-open (car ls))
    (match-line (cdr ls) (cons (car ls) stack)))
   (else (pop ls stack))))

(define (solve board)
  (fold + 0
        (map (lambda (l) (match-line l '())) board)))

(define (main _)
  (display (solve (board)))
  (newline))

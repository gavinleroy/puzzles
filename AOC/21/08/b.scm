#!/usr/local/bin/guile \
-e main -s
!#

;; Gavin Gray AOC 21

(use-modules (ice-9 textual-ports)
             (ice-9 common-list)
             (srfi srfi-1))

(define stdin (current-input-port))
(define (lines s) (string-split s #\newline))
(define (split-pipe s) (string-split s #\|))
(define (split-space s) (string-split (string-trim-both s) #\space))

(define-syntax-rule (all) (get-string-all stdin))
(define-syntax-rule (pair p1 p2) (cons p1 `(,p2)))
(define-syntax-rule (snd p) (car (cdr p)))
(define-syntax-rule (fst p) (car p))
(define-syntax-rule (xor l1 l2) (set-difference (union l1 l2) (intersection l1 l2)))
;; I'm positive the different finds could be coupled into one
;; fantastic macro but I don't have the time to tinker with it
(define-syntax-rule (find-simple n l dcdr data)
  (pair n (find-if (lambda (v) (= l (length v))) data)))
(define-syntax-rule (find-cmp-1 c n dcdr data)
  (let ((num (get c dcdr)))
    (pair n (find-if (lambda (v)
                       (let ((ve (xor v num)))
                         (= 1 (length ve)))) data))))

(define (break inp)
  (map (lambda (l)
         (let* ((lp (split-pipe l))
                (f (car lp))
                (s (car (cdr lp)))
                (fun (lambda (s)
                       (map string->list (split-space s)))))
           (pair (fun f) (fun s)))) (lines (string-trim-right inp))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define (get n dcdr)
  (snd (find-if (lambda (v)
                  (= n (car v))) dcdr)))

(define (decode digit dcdr)
  (car (find-if (lambda (v)
                  (equal? (sort (snd v) char<=?)
                          (sort digit char<=?))) dcdr)))

(define (filtered n nums)
  (filter (lambda (d)
            (not (equal? (snd n) d))) nums))

;; NOTE each of these has specific expectations to which  numbers are available.
(define (find-zero  dcdr data) (find-simple 0 6 dcdr data))
(define (find-one   dcdr data) (find-simple 1 2 dcdr data))
(define (find-two   dcdr data) (find-simple 2 5 dcdr data))
(define (find-four  dcdr data) (find-simple 4 4 dcdr data))
(define (find-five  dcdr data) (find-cmp-1 6 5 dcdr data))
(define (find-seven dcdr data) (find-simple 7 3 dcdr data))
(define (find-eight dcdr data) (find-simple 8 7 dcdr data))
(define (find-nine  dcdr data) (find-cmp-1 3 9 dcdr data))

(define (find-six   dcdr data)
  (let ((one (get 1 dcdr))
        (eight (get 8 dcdr)))
    (pair 6
          (find-if (lambda (v)
                     (let ((ve (xor v eight)))
                       (and (= 1 (length ve))
                            (= 1 (length
                                  (xor ve one)))))) data))))

(define (find-three dcdr data)
  (let ((one (get 1 dcdr))
        (eight (get 8 dcdr)))
    (pair 3
          (find-if (lambda (v)
                     (let ((ve (xor v eight)))
                       (and (= 2 (length ve))
                            (= 4 (length
                                  (xor ve one)))))) data))))

(define (decode-line data)
  (let* ((nums (fst data))
        (to-decode (snd data))
        (dcdr (fst (fold (lambda (f acc)
                           (let ((n (f (fst acc)
                                       (snd acc))))
                             (pair (cons n (fst acc))
                                   (filtered n (snd acc)))))
                         `('() ,nums) `(,find-one
                                        ,find-four
                                        ,find-seven
                                        ,find-eight
                                        ,find-six
                                        ,find-three
                                        ,find-five
                                        ,find-nine
                                        ,find-zero
                                        ,find-two)))))
    (fold (lambda (v acc)
            (+ (* acc 10) v)) 0
            (map (lambda (d)
                   (decode d dcdr)) to-decode))))

(define-syntax-rule (solve)
  (reduce + 0
          (map decode-line (break (all)))))

(define (main _)
  (display (solve))
  (newline))

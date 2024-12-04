(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '()))

(defpackage :day04
  (:use :cl))

(in-package :day04)

;; --- Day 4: Ceres Search ---
;; "Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!

;; As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.

;; This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:


;; ..X...
;; .SAMX.
;; .A..A.
;; XMAS.S
;; .X....
;; The actual word search will be full of letters instead. For example:

;; MMMSXXMASM
;; MSAMXMSMSA
;; AMXSXMAAMM
;; MSAMASMSMX
;; XMASAMXAMM
;; XXAMMXXAMA
;; SMSMSASXSS
;; SAXAMASAAA
;; MAMMMXMMMM
;; MXMXAXMASX
;; In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:

;; ....XXMAS.
;; .SAMXMS...
;; ...S..A...
;; ..A.A.MS.X
;; XMASAMX.MM
;; X.....XA.A
;; S.S.S.S.SS
;; .A.A.A.A.A
;; ..M.M.M.MM
;; .X.X.XMASX
;; Take a look at the little Elf's word search. How many times does XMAS appear?

(defun read-input (filename)
  (with-open-file (stream filename)
    (let* ((lines (loop for line = (read-line stream nil)
                        while line
                        collecting (string-upcase line)))
           (m (length lines))
           (n (length (car lines))))
      (values (make-array (list m n)
                          :initial-contents
                          (mapcar (lambda (s) (coerce s 'vector)) lines))
              m n))))

(defconstant *XMAS* (coerce "XMAS" 'list))

(defun find-sources (mat m n &optional (cc #\X))
  (loop for i from 0 below m
        append
        (loop for j from 0 below n
              for c = (aref mat i j)
              when (eq c CC)
                collect (cons i j))))

(defun xmasp (mat m n i j di dj &optional (xmas *XMAS*))
  (let ((word (loop for ii = i then (+ ii di)
                    for jj = j then (+ jj dj)
                    for _ in xmas
                    when (and (<= 0 ii) (< ii m)
                              (<= 0 jj) (< jj n))
                      collect (aref mat ii jj))))
    (or (equal word xmas)
        (equal word (reverse xmas)))))

(defun part-1 ()
  (multiple-value-bind (mat m n) (read-input "input.data")
    (loop for (i . j) in (find-sources mat m n)
          sum
          (loop for (di . dj) in '(( 0 .  1)
                                   ( 0 . -1)
                                   ( 1 .  0)
                                   (-1 .  0)
                                   ( 1 .  1)
                                   (-1 . -1)
                                   (-1 .  1)
                                   ( 1 . -1))
                count (xmasp mat m n i j di dj)))))

;; --- Part Two ---
;; The Elf looks quizzically at you. Did you misunderstand the assignment?

;; Looking for the instructions, you flip over the word search to find that this isn't actually an XMAS puzzle; it's an X-MAS puzzle in which you're supposed to find two MAS in the shape of an X. One way to achieve that is like this:

;; M.S
;; .A.
;; M.S
;; Irrelevant characters have again been replaced with . in the above diagram. Within the X, each MAS can be written forwards or backwards.

;; Here's the same example from before, but this time all of the X-MASes have been kept instead:

;; .M.S......
;; ..A..MSMS.
;; .M.S.MAA..
;; ..A.ASMSM.
;; .M.S.M....
;; ..........
;; S.S.S.S.S.
;; .A.A.A.A..
;; M.M.M.M.M.
;; ..........
;; In this example, an X-MAS appears 9 times.

;; Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?

(defun part-2 ()
  (multiple-value-bind (mat m n) (read-input "input.data")
    (loop for (i . j) in (find-sources mat m n #\A)
          count (and (xmasp mat m n (- i 1) (- j 1)  1  1 (cdr *XMAS*))
                     (xmasp mat m n (+ i 1) (- j 1) -1  1 (cdr *XMAS*))))))

(defun main ()
  (format t "Part 1: ~a~%" (part-1))
  (format t "Part 2: ~a~%" (part-2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:str :fset)))

(defpackage :day02
  (:use :cl :str))

(in-package :day02)

;; --- Day 2: Red-Nosed Reports ---
;; Fortunately, the first location The Historians want to search isn't a long walk from the Chief Historian's office.

;; While the Red-Nosed Reindeer nuclear fusion/fission plant appears to contain no sign of the Chief Historian, the engineers there run up to you as soon as they see you. Apparently, they still talk about the time Rudolph was saved through molecular synthesis from a single electron.

;; They're quick to add that - since you're already here - they'd really appreciate your help analyzing some unusual data from the Red-Nosed reactor. You turn to check if The Historians are waiting for you, but they seem to have already divided into groups that are currently searching every corner of the facility. You offer to help with the unusual data.

;; The unusual data (your puzzle input) consists of many reports, one report per line. Each report is a list of numbers called levels that are separated by spaces. For example:

;; 7 6 4 2 1
;; 1 2 7 8 9
;; 9 7 6 2 1
;; 1 3 2 4 5
;; 8 6 4 4 1
;; 1 3 6 7 9
;; This example data contains six reports each containing five levels.

;; The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:

;; The levels are either all increasing or all decreasing.
;; Any two adjacent levels differ by at least one and at most three.
;; In the example above, the reports can be found safe or unsafe by checking those rules:

;; 7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
;; 1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
;; 9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
;; 1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
;; 8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
;; 1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.
;; So, in this example, 2 reports are safe.

;; Analyze the unusual data from the engineers. How many reports are safe?

(defun read-input (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect
          (mapcar #'parse-integer (words line)))))

(defun safe-report-p (report)
  (let ((differences (mapcar (lambda (a b) (- b a)) report (cdr report))))
    (and (or (every #'plusp differences)
             (every #'minusp differences))
         (every (lambda (x) (<= 1 (abs x) 3)) differences))))


(defun part-1 ()
  (length (remove-if-not #'safe-report-p (read-input "input.data"))))

;; --- Part Two ---
;; The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.

;; The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!

;; Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.

;; More of the above example's reports are now safe:

;; 7 6 4 2 1: Safe without removing any level.
;; 1 2 7 8 9: Unsafe regardless of which level is removed.
;; 9 7 6 2 1: Unsafe regardless of which level is removed.
;; 1 3 2 4 5: Safe by removing the second level, 3.
;; 8 6 4 4 1: Safe by removing the third level, 4.
;; 1 3 6 7 9: Safe without removing any level.
;; Thanks to the Problem Dampener, 4 reports are actually safe!

;; Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe reports. How many reports are now safe?

(defun remove-at (list i)
  (loop for j from 0 below (length list)
        unless (= i j)
          collect (nth j list)))

(defun safe-report-p-2 (report)
  (when (safe-report-p report)
    (return-from safe-report-p-2 t))
  ;; For each index i, remove the ith element and check if the report is safe
  (loop for i from 0 below (length report)
        for new-report = (remove-at report i)
        when (safe-report-p new-report)
          return t))

(defun part-2 ()
  (length (remove-if-not #'safe-report-p-2 (read-input "input.data"))))

(defun main ()
  (format t "Part 1: ~a~%" (part-1))
  (format t "Part 2: ~a~%" (part-2)))

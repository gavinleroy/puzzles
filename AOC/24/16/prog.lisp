(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:priority-queue :fset)))

(defpackage :day16
  (:import-from :priority-queue #:make-pqueue #:pqueue-push #:pqueue-pop #:pqueue-empty-p)
  (:use :cl))

(in-package :day16)

;; --- Day 16: Reindeer Maze ---
;; It's time again for the Reindeer Olympics! This year, the big event is the Reindeer Maze, where the Reindeer compete for the lowest score.

;; You and The Historians arrive to search for the Chief right as the event is about to start. It wouldn't hurt to watch a little, right?

;; The Rmindeer start on the Start Tile (marked S) facing East and need to reach the End Tile (marked E). They can move forward one tile at a time (increasing their score by 1 point), but never into a wall (#). They can also rotate clockwise or counterclockwise 90 degrees at a time (increasing their score by 1000 points).

;; To figure out the best place to sit, you start by grabbing a map (your puzzle input) from a nearby kiosk. For example:

;; ###############
;; #.......#....E#
;; #.#.###.#.###.#
;; #.....#.#...#.#
;; #.###.#####.#.#
;; #.#.#.......#.#
;; #.#.#####.###.#
;; #...........#.#
;; ###.#.#####.#.#
;; #...#.....#.#.#
;; #.#.#.###.#.#.#
;; #.....#...#.#.#
;; #.###.#.#.#.#.#
;; #S..#.....#...#
;; ###############
;; There are many paths through this maze, but taking any of the best paths would incur a score of only 7036. This can be achieved by taking a total of 36 steps forward and turning 90 degrees a total of 7 times:


;; ###############
;; #.......#....E#
;; #.#.###.#.###^#
;; #.....#.#...#^#
;; #.###.#####.#^#
;; #.#.#.......#^#
;; #.#.#####.###^#
;; #..>>>>>>>>v#^#
;; ###^#.#####v#^#
;; #>>^#.....#v#^#
;; #^#.#.###.#v#^#
;; #^....#...#v#^#
;; #^###.#.#.#v#^#
;; #S..#.....#>>^#
;; ###############
;; Here's a second example:

;; #################
;; #...#...#...#..E#
;; #.#.#.#.#.#.#.#.#
;; #.#.#.#...#...#.#
;; #.#.#.#.###.#.#.#
;; #...#.#.#.....#.#
;; #.#.#.#.#.#####.#
;; #.#...#.#.#.....#
;; #.#.#####.#.###.#
;; #.#.#.......#...#
;; #.#.###.#####.###
;; #.#.#...#.....#.#
;; #.#.#.#####.###.#
;; #.#.#.........#.#
;; #.#.#.#########.#
;; #S#.............#
;; #################
;; In this maze, the best paths cost 11048 points; following one such path would look like this:

;; #################
;; #...#...#...#..E#
;; #.#.#.#.#.#.#.#^#
;; #.#.#.#...#...#^#
;; #.#.#.#.###.#.#^#
;; #>>v#.#.#.....#^#
;; #^#v#.#.#.#####^#
;; #^#v..#.#.#>>>>^#
;; #^#v#####.#^###.#
;; #^#v#..>>>>^#...#
;; #^#v###^#####.###
;; #^#v#>>^#.....#.#
;; #^#v#^#####.###.#
;; #^#v#^........#.#
;; #^#v#^#########.#
;; #S#>>^..........#
;; #################
;; Note that the path shown above includes one 90 degree turn as the very first move, rotating the Reindeer from facing East to facing North.

;; Analyze your map carefully. What is the lowest score a Reindeer could possibly get?

(defun read-input (filename)
  (flet ((concat-all (ls) (format nil "~{~a~}" ls))
         (to-row-major (r c mc) (+ (* mc r) c))
         (to-x-y (v width) (list (floor v width) (mod v width))))
    (multiple-value-bind (mr mc it)
        (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                for r from 0
                while line
                maximizing r into mr
                maximizing (length line) into mc
                collect line into it
                finally (return (values mr mc it))))
      (let* ((s (concat-all it))
             (start (to-x-y (position #\S s) mc))
             (end (to-x-y (position #\E s) mc))
             (getter (lambda (r c)
                       (when (and (< -1 r mr) (< -1 c mc))
                         (aref s (to-row-major r c mc))))))
        (values start end getter)))))

(defconstant +dirs+
  '#1=((0 1) (1 0) (0 -1) (-1 0) . #1#))

(defun left (dir) (cdddr dir))
(defun right (dir) (cdr dir))

(defun pos+ (p1 p2)
  (list (+ (first p1) (first p2))
        (+ (second p1) (second p2))))

(defun neighbors (n get)
  (loop with (p d) = n
        for dd in (list d (left d) (right d))
        for cost in (list 1 1001 1001)
        for dp = (pos+ p (first dd))
        for at = (funcall get (first dp) (second dp))
        when at
          unless (char= at #\#)
            collect (list (list dp dd) cost)))

(defun dijkstra (get source)
  (let* ((dist (make-hash-table :test #'equal))
         (prev (make-hash-table :test #'equal))
         (q (make-pqueue #'<))
         (start (list source +dirs+)))
    (pqueue-push start (setf (gethash start dist) 0) q)
    (loop until (pqueue-empty-p q)
          for u = (pqueue-pop q)
          do (loop for (v cost) in (neighbors u get)
                   for alt = (+ (gethash u dist) cost)
                   for vdist = (gethash v dist)
                   if (or (not vdist) (< alt vdist))
                     do (setf (gethash v dist) alt)
                        (pqueue-push v alt q)
                        (setf (gethash v prev) (list u))
                   else
                     if (= alt vdist)
                       do (pqueue-push v alt q)
                          (setf (gethash v prev)
                                (cons u (gethash v prev))))
          finally (return (values dist prev)))))


(defun part-1 (E dist)
  (loop repeat 4
        for d on +dirs+
        for at = (gethash (list E d) dist)
        when at
          minimize at))

;; --- Part Two ---
;; Now that you know what the best paths look like, you can figure out the best spot to sit.

;; Every non-wall tile (S, ., or E) is equipped with places to sit along the edges of the tile. While determining which of these tiles would be the best spot to sit depends on a whole bunch of factors (how comfortable the seats are, how far away the bathrooms are, whether there's a pillar blocking your view, etc.), the most important factor is whether the tile is on one of the best paths through the maze. If you sit somewhere else, you'd miss all the action!

;; So, you'll need to determine which tiles are part of any best path through the maze, including the S and E tiles.

;; In the first example, there are 45 tiles (marked O) that are part of at least one of the various best paths through the maze:

;; ###############
;; #.......#....O#
;; #.#.###.#.###O#
;; #.....#.#...#O#
;; #.###.#####.#O#
;; #.#.#.......#O#
;; #.#.#####.###O#
;; #..OOOOOOOOO#O#
;; ###O#O#####O#O#
;; #OOO#O....#O#O#
;; #O#O#O###.#O#O#
;; #OOOOO#...#O#O#
;; #O###.#.#.#O#O#
;; #O..#.....#OOO#
;; ###############
;; In the second example, there are 64 tiles that are part of at least one of the best paths:

;; #################
;; #...#...#...#..O#
;; #.#.#.#.#.#.#.#O#
;; #.#.#.#...#...#O#
;; #.#.#.#.###.#.#O#
;; #OOO#.#.#.....#O#
;; #O#O#.#.#.#####O#
;; #O#O..#.#.#OOOOO#
;; #O#O#####.#O###O#
;; #O#O#..OOOOO#OOO#
;; #O#O###O#####O###
;; #O#O#OOO#..OOO#.#
;; #O#O#O#####O###.#
;; #O#O#OOOOOOO..#.#
;; #O#O#O#########.#
;; #O#OOO..........#
;; #################
;;
;; Analyze your map further. How many tiles are part of at least one of the best paths through the maze?

(defun traverse-paths (prev roots)
  (let ((to-visit (fset:convert 'fset:set roots))
        (places (fset:empty-set)))
    (loop until (fset:empty? to-visit)
          do (setf places
                   (fset:union places (fset:image #'first to-visit))
                   to-visit
                   (fset:reduce (lambda (acc n)
                                  (reduce #'fset:with (gethash n prev)
                                          :initial-value acc))
                                to-visit :initial-value (fset:empty-set)))
          finally (return (fset:size places)))))

(defun part-2 (E dist prev)
  (loop repeat 4
        for d on +dirs+
        for n = (list E d)
        for cost = (gethash n dist)
        when cost
          minimize cost into min
          and collect (cons n cost) into res
        finally
           (return
             (traverse-paths prev (mapcar #'first (remove-if-not (lambda (x) (= (cdr x) min)) res))))))

(defun main ()
  (multiple-value-bind (S E get) (read-input "input.data")
    (multiple-value-bind (dist prev) (dijkstra get S)
      (format t "Part 1: ~a~%" (part-1 E dist))
      (format t "Part 2: ~a~%" (part-2 E dist prev)))))

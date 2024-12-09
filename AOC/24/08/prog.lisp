(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fset)))

(defpackage :day08
  (:import-from :fset #:with #:@ #:compare)
  (:use :cl))

(in-package :day08)

;; --- Day 8: Resonant Collinearity ---
;; You find yourselves on the roof of a top-secret Easter Bunny installation.

;; While The Historians do their thing, you take a look at the familiar huge antenna. Much to your surprise, it seems to have been reconfigured to emit a signal that makes people 0.1% more likely to buy Easter Bunny brand Imitation Mediocre Chocolate as a Christmas gift! Unthinkable!

;; Scanning across the city, you find that there are actually many such antennas. Each antenna is tuned to a specific frequency indicated by a single lowercase letter, uppercase letter, or digit. You create a map (your puzzle input) of these antennas. For example:

;; ............
;; ........0...
;; .....0......
;; .......0....
;; ....0.......
;; ......A.....
;; ............
;; ............
;; ........A...
;; .........A..
;; ............
;; ............
;; The signal only applies its nefarious effect at specific antinodes based on the resonant frequencies of the antennas. In particular, an antinode occurs at any point that is perfectly in line with two antennas of the same frequency - but only when one of the antennas is twice as far away as the other. This means that for any pair of antennas with the same frequency, there are two antinodes, one on either side of them.

;; So, for these two antennas with frequency a, they create the two antinodes marked with #:

;; ..........
;; ...#......
;; ..........
;; ....a.....
;; ..........
;; .....a....
;; ..........
;; ......#...
;; ..........
;; ..........
;; Adding a third antenna with the same frequency creates several more antinodes. It would ideally add four antinodes, but two are off the right side of the map, so instead it adds only two:

;; ..........
;; ...#......
;; #.........
;; ....a.....
;; ........a.
;; .....a....
;; ..#.......
;; ......#...
;; ..........
;; ..........
;; Antennas with different frequencies don't create antinodes; A and a count as different frequencies. However, antinodes can occur at locations that contain antennas. In this diagram, the lone antenna with frequency capital A creates no antinodes but has a lowercase-a-frequency antinode at its location:

;; ..........
;; ...#......
;; #.........
;; ....a.....
;; ........a.
;; .....a....
;; ..#.......
;; ......A...
;; ..........
;; ..........
;; The first example has antennas with two different frequencies, so the antinodes they create look like this, plus an antinode overlapping the topmost A-frequency antenna:

;; ......#....#
;; ...#....0...
;; ....#0....#.
;; ..#....0....
;; ....0....#..
;; .#....A.....
;; ...#........
;; #......#....
;; ........A...
;; .........A..
;; ..........#.
;; ..........#.
;; Because the topmost A-frequency antenna overlaps with a 0-frequency antinode, there are 14 total unique locations that contain an antinode within the bounds of the map.

;; Calculate the impact of the signal. How many unique locations within the bounds of the map contain an antinode?

(defclass pos ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)))

(defun make-pos (x y)
  (make-instance 'pos :x x :y y))

(defmethod print-object ((obj pos) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (x y) obj
      (format stream "(~a, ~a)" x y))))

(defclass move (pos)
  ())

(defmethod distance ((a pos) (b pos))
  (let ((dx (- (x a) (x b)))
        (dy (- (y a) (y b))))
    (make-instance 'move :x dx :y dy)))

(defmethod boundsp ((a pos) (mx integer) (my integer))
  (and (<= 0 (x a) mx)
       (<= 0 (y a) my)))

(defmethod add ((a pos) (m move))
  (make-pos (+ (x a) (x m)) (+ (y a) (y m))))

(defmethod minus ((a pos) (m move))
  (make-pos (- (x a) (x m)) (- (y a) (y m))))

(defmethod antinodes ((a pos) (b pos) (mx integer) (my integer) &key harmonics)
  (let* ((m (distance a b))
         (aa (add a m))
         (bb (minus b m)))
    (fset:set
     (when (boundsp aa mx my) aa)
     (when (boundsp bb mx my) bb))))

(defmethod compare ((p1 pos) (p2 pos))
  (fset:compare-slots p1 p2 'x 'y))

(defun read-input (filename)
  (let ((points (fset:empty-map (fset:empty-set)))
        (mx))
    (flet ((npush (c i j) (setf points (with points c (with (@ points c) (make-instance 'pos :x j :y i))))))
      (with-open-file (stream filename)
        (loop for i from 0
              for line = (read-line stream nil)
              while line
              maximize i into my
              do (loop for j from 0
                       for c across line
                       maximize j into mxx
                       unless (char= c #\.)
                         do (npush c i j)
                       finally (setf mx mxx))
              finally (return (values points mx my)))))))

(defun run (points mx my &key (harmonics nil))
  (let ((ans (fset:empty-set)))
    (fset:do-map (c s points)
      (declare (ignore c))
      (fset:do-set (a s)
        (fset:do-set (b s)
          (unless (eq a b)
            (fset:unionf ans (antinodes a b mx my :harmonics harmonics))))))
    (fset:size (fset:less ans nil))))

;; --- Part Two ---
;; Watching over your shoulder as you work, one of The Historians asks if you took the effects of resonant harmonics into your calculations.

;; Whoops!

;; After updating your model, it turns out that an antinode occurs at any grid position exactly in line with at least two antennas of the same frequency, regardless of distance. This means that some of the new antinodes will occur at the position of each antenna (unless that antenna is the only one of its frequency).

;; So, these three T-frequency antennas now create many antinodes:

;; T....#....
;; ...T......
;; .T....#...
;; .........#
;; ..#.......
;; ..........
;; ...#......
;; ..........
;; ....#.....
;; ..........
;; In fact, the three T-frequency antennas are all exactly in line with two antennas, so they are all also antinodes! This brings the total number of antinodes in the above example to 9.

;; The original example now has 34 antinodes, including the antinodes that appear on every antenna:

;; ##....#....#
;; .#.#....0...
;; ..#.#0....#.
;; ..##...0....
;; ....0....#..
;; .#...#A....#
;; ...#..#.....
;; #....#.#....
;; ..#.....A...
;; ....#....A..
;; .#........#.
;; ...#......##
;; Calculate the impact of the signal using this updated model. How many unique locations within the bounds of the map contain an antinode?

(defmethod antinodes :around ((a pos) (b pos) (mx integer) (my integer) &key harmonics)
  (when (next-method-p)
    (let ((ns (call-next-method))
          (m (distance a b)))
      (when harmonics
        (with-slots (x y) m
          (flet ((shoot (start f)
                   (loop for p = start then (funcall f p m)
                         while (boundsp p mx my)
                         do (setf ns (with ns p)))))
            (shoot a #'add)
            (shoot b #'minus))))
      ns)))


(defun main ()
  (multiple-value-bind (points mx my)
      (read-input "input.data")
    (format t "Part 1: ~a~%" (run points mx my))
    (format t "Part 2: ~a~%" (run points mx my :harmonics t))))

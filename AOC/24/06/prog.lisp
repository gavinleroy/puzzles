(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fset)))

(defpackage :day06
  (:import-from :fset #:@)
  (:use :cl))

(in-package :day06)

;; --- Day 6: Guard Gallivant ---
;; The Historians use their fancy device again, this time to whisk you all away to the North Pole prototype suit manufacturing lab... in the year 1518! It turns out that having direct access to history is very convenient for a group of historians.

;; You still have to be careful of time paradoxes, and so it will be important to avoid anyone from 1518 while The Historians search for the Chief. Unfortunately, a single guard is patrolling this part of the lab.

;; Maybe you can work out where the guard will go ahead of time so that The Historians can search safely?

;; You start by making a map (your puzzle input) of the situation. For example:

;; ....#.....
;; .........#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#..^.....
;; ........#.
;; #.........
;; ......#...
;; The map shows the current position of the guard with ^ (to indicate the guard is currently facing up from the perspective of the map). Any obstructions - crates, desks, alchemical reactors, etc. - are shown as #.

;; Lab guards in 1518 follow a very strict patrol protocol which involves repeatedly following these steps:

;; If there is something directly in front of you, turn right 90 degrees.
;; Otherwise, take a step forward.
;; Following the above protocol, the guard moves up several times until she reaches an obstacle (in this case, a pile of failed suit prototypes):

;; ....#.....
;; ....^....#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#........
;; ........#.
;; #.........
;; ......#...
;; Because there is now an obstacle in front of the guard, she turns right before continuing straight in her new facing direction:

;; ....#.....
;; ........>#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#........
;; ........#.
;; #.........
;; ......#...
;; Reaching another obstacle (a spool of several very long polymers), she turns right again and continues downward:

;; ....#.....
;; .........#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#......v.
;; ........#.
;; #.........
;; ......#...
;; This process continues for a while, but the guard eventually leaves the mapped area (after walking past a tank of universal solvent):

;; ....#.....
;; .........#
;; ..........
;; ..#.......
;; .......#..
;; ..........
;; .#........
;; ........#.
;; #.........
;; ......#v..
;; By predicting the guard's route, you can determine which specific positions in the lab will be in the patrol path. Including the guard's starting position, the positions visited by the guard before leaving the area are marked with an X:

;; ....#.....
;; ....XXXXX#
;; ....X...X.
;; ..#.X...X.
;; ..XXXXX#X.
;; ..X.X.X.X.
;; .#XXXXXXX.
;; .XXXXXXX#.
;; #XXXXXXX..
;; ......#X..
;; In this example, the guard will visit 41 distinct positions on your map.

;; Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?

(defun read-input (filename)
  (let ((guard-pos) (max-col))
    (with-open-file (stream filename)
      (loop for i from 0
            for line = (read-line stream nil)
            maximize i into max-row
            while line
            collecting
            (loop for j from 0
                  for c across line
                  do (setf max-col j)
                  when (char= c #\^)
                    do (setf guard-pos (cons i j))
                  when  (char= c #\#)
                    collect j into nested
                  finally (return (fset:map (i (reduce #'fset:with nested
                                                       :initial-value (fset:empty-set)))
                                            :default (fset:empty-set))))
              into it
            finally (return (values (reduce #'fset:map-union it)
                                    guard-pos
                                    max-row
                                    max-col))))))

(defconstant +dirs+
  '#1=((-1 . 0) (0 . 1) (1 . 0) (0 . -1) . #1#))

(defun occupiedp (i j objs)
  (declare (optimize (speed 3) (safety 0)))
  (fset:contains? (@ objs i) j))

(defun walk (walls guard-pos mi mj &optional (dirs +dirs+))
  (declare (optimize (speed 3) (safety 0)))
  (let ((positions)
        (turns))
    (loop for (i . j) = guard-pos
          for (di . dj) = (car dirs)
          for ii = (+ i di)
          for jj = (+ j dj)
          for point = (cons guard-pos dirs)
          while (and (< -1 i mi) (< -1 j mj))
          when (member point turns :test #'equal)
            do (return-from walk nil) ;; loop detected
          do (push point positions)
          if (occupiedp ii jj walls)
            do (progn (push point turns)
                      (setf dirs (cdr dirs)))
          else
            do (setf guard-pos (cons ii jj))
          finally (return positions))))

(defun part-1 (walls start mi mj)
  (length
   (remove-duplicates
    (walk walls start mi mj)
    :key #'car :test #'equal)))

;; --- Part Two ---

;; While The Historians begin working around the guard's patrol route, you borrow their fancy device and step outside the lab. From the safety of a supply closet, you time travel through the last few months and record the nightly status of the lab's guard post on the walls of the closet.

;; Returning after what seems like only a few seconds to The Historians, they explain that the guard's patrol area is simply too large for them to safely search the lab without getting caught.

;; Fortunately, they are pretty sure that adding a single new obstruction won't cause a time paradox. They'd like to place the new obstruction in such a way that the guard will get stuck in a loop, making the rest of the lab safe to search.

;; To have the lowest chance of creating a time paradox, The Historians would like to know all of the possible positions for such an obstruction. The new obstruction can't be placed at the guard's starting position - the guard is there right now and would notice.

;; In the above example, there are only 6 different positions where a new obstruction would cause the guard to get stuck in a loop. The diagrams of these six situations use O to mark the new obstruction, | to show a position where the guard moves up/down, - to show a position where the guard moves left/right, and + to show a position where the guard moves both up/down and left/right.

;; Option one, put a printing press next to the guard's starting position:

;; ....#.....
;; ....+---+#
;; ....|...|.
;; ..#.|...|.
;; ....|..#|.
;; ....|...|.
;; .#.O^---+.
;; ........#.
;; #.........
;; ......#...
;; Option two, put a stack of failed suit prototypes in the bottom right quadrant of the mapped area:


;; ....#.....
;; ....+---+#
;; ....|...|.
;; ..#.|...|.
;; ..+-+-+#|.
;; ..|.|.|.|.
;; .#+-^-+-+.
;; ......O.#.
;; #.........
;; ......#...
;; Option three, put a crate of chimney-squeeze prototype fabric next to the standing desk in the bottom right quadrant:

;; ....#.....
;; ....+---+#
;; ....|...|.
;; ..#.|...|.
;; ..+-+-+#|.
;; ..|.|.|.|.
;; .#+-^-+-+.
;; .+----+O#.
;; #+----+...
;; ......#...
;; Option four, put an alchemical retroencabulator near the bottom left corner:

;; ....#.....
;; ....+---+#
;; ....|...|.
;; ..#.|...|.
;; ..+-+-+#|.
;; ..|.|.|.|.
;; .#+-^-+-+.
;; ..|...|.#.
;; #O+---+...
;; ......#...
;; Option five, put the alchemical retroencabulator a bit to the right instead:

;; ....#.....
;; ....+---+#
;; ....|...|.
;; ..#.|...|.
;; ..+-+-+#|.
;; ..|.|.|.|.
;; .#+-^-+-+.
;; ....|.|.#.
;; #..O+-+...
;; ......#...
;; Option six, put a tank of sovereign glue right next to the tank of universal solvent:

;; ....#.....
;; ....+---+#
;; ....|...|.
;; ..#.|...|.
;; ..+-+-+#|.
;; ..|.|.|.|.
;; .#+-^-+-+.
;; .+----++#.
;; #+----++..
;; ......#O..
;; It doesn't really matter what you choose to use as an obstacle so long as you and The Historians can put it into position without the guard noticing. The important thing is having enough options that you can find one that minimizes time paradoxes, and in this example, there are 6 different positions you could choose.

;; You need to get the guard stuck in a loop by adding a single new obstruction. How many different positions could you choose for this obstruction?

(defun part-2 (walls start mi mj)
  (declare (optimize (speed 3) (safety 0)))
  (flet ((with-pos (i j objs)
           (fset:with objs i (fset:with (@ objs i) j)))
         (first-two (ls)
           (cons (car ls) (cadr ls)))
         (pos-minus (p dir)
           (cons
            (- (car p) (car dir))
            (- (cdr p) (cdr dir)))))
    (let ((initial-path
            (remove-duplicates (walk walls start mi mj)
                               :key #'car :test #'equal)))
      (loop for count from 0
            for (p . dirs) in initial-path
            for (i . j) = p
            unless (or (equalp start p)
                       (walk (with-pos i j walls)
                             (pos-minus p (car dirs)) mi mj
                             dirs))
              sum 1))))

;; 2s runtime, definitely could be faster
(defun main ()
  (multiple-value-bind (walls start mi mj)
      (read-input "input.data")
    (format t "Part 1: ~a~%" (part-1 walls start mi mj))
    (format t "Part 2: ~a~%" (part-2 walls start mi mj))))

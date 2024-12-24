(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fset)))

(defpackage :day21
  (:use :cl))

(in-package :day21)

;; --- Day 21: Keypad Conundrum ---
;; As you teleport onto Santa's Reindeer-class starship, The Historians begin to panic: someone from their search party is missing. A quick life-form scan by the ship's computer reveals that when the missing Historian teleported, he arrived in another part of the ship.

;; The door to that area is locked, but the computer can't open it; it can only be opened by physically typing the door codes (your puzzle input) on the numeric keypad on the door.

;; The numeric keypad has four rows of buttons: 789, 456, 123, and finally an empty gap followed by 0A. Visually, they are arranged like this:

;; +---+---+---+
;; | 7 | 8 | 9 |
;; +---+---+---+
;; | 4 | 5 | 6 |
;; +---+---+---+
;; | 1 | 2 | 3 |
;; +---+---+---+
;;     | 0 | A |
;;     +---+---+
;; Unfortunately, the area outside the door is currently depressurized and nobody can go near the door. A robot needs to be sent instead.

;; The robot has no problem navigating the ship and finding the numeric keypad, but it's not designed for button pushing: it can't be told to push a specific button directly. Instead, it has a robotic arm that can be controlled remotely via a directional keypad.

;; The directional keypad has two rows of buttons: a gap / ^ (up) / A (activate) on the first row and < (left) / v (down) / > (right) on the second row. Visually, they are arranged like this:

;;     +---+---+
;;     | ^ | A |
;; +---+---+---+
;; | < | v | > |
;; +---+---+---+
;; When the robot arrives at the numeric keypad, its robotic arm is pointed at the A button in the bottom right corner. After that, this directional keypad remote control must be used to maneuver the robotic arm: the up / down / left / right buttons cause it to move its arm one button in that direction, and the A button causes the robot to briefly move forward, pressing the button being aimed at by the robotic arm.

;; For example, to make the robot type 029A on the numeric keypad, one sequence of inputs on the directional keypad you could use is:

;; < to move the arm from A (its initial position) to 0.
;; A to push the 0 button.
;; ^A to move the arm to the 2 button and push it.
;; >^^A to move the arm to the 9 button and push it.
;; vvvA to move the arm to the A button and push it.
;; In total, there are three shortest possible sequences of button presses on this directional keypad that would cause the robot to type 029A: <A^A>^^AvvvA, <A^A^>^AvvvA, and <A^A^^>AvvvA.

;; Unfortunately, the area containing this directional keypad remote control is currently experiencing high levels of radiation and nobody can go near it. A robot needs to be sent instead.

;; When the robot arrives at the directional keypad, its robot arm is pointed at the A button in the upper right corner. After that, a second, different directional keypad remote control is used to control this robot (in the same way as the first robot, except that this one is typing on a directional keypad instead of a numeric keypad).

;; There are multiple shortest possible sequences of directional keypad button presses that would cause this robot to tell the first robot to type 029A on the door. One such sequence is v<<A>>^A<A>AvA<^AA>A<vAAA>^A.

;; Unfortunately, the area containing this second directional keypad remote control is currently -40 degrees! Another robot will need to be sent to type on that directional keypad, too.

;; There are many shortest possible sequences of directional keypad button presses that would cause this robot to tell the second robot to tell the first robot to eventually type 029A on the door. One such sequence is <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A.

;; Unfortunately, the area containing this third directional keypad remote control is currently full of Historians, so no robots can find a clear path there. Instead, you will have to type this sequence yourself.

;; Were you to choose this sequence of button presses, here are all of the buttons that would be pressed on your directional keypad, the two robots' directional keypads, and the numeric keypad:

;; <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
;; v<<A>>^A<A>AvA<^AA>A<vAAA>^A
;; <A^A>^^AvvvA
;; 029A
;; In summary, there are the following keypads:

;; One directional keypad that you are using.
;; Two directional keypads that robots are using.
;; One numeric keypad (on a door) that a robot is using.
;; It is important to remember that these robots are not designed for button pushing. In particular, if a robot arm is ever aimed at a gap where no button is present on the keypad, even for an instant, the robot will panic unrecoverably. So, don't do that. All robots will initially aim at the keypad's A key, wherever it is.

;; To unlock the door, five codes will need to be typed on its numeric keypad. For example:

;; 029A
;; 980A
;; 179A
;; 456A
;; 379A
;; For each of these, here is a shortest sequence of button presses you could type to cause the desired code to be typed on the numeric keypad:

;; 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
;; 980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
;; 179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
;; 456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
;; 379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
;; The Historians are getting nervous; the ship computer doesn't remember whether the missing Historian is trapped in the area containing a giant electromagnet or molten lava. You'll need to make sure that for each of the five codes, you find the shortest sequence of button presses necessary.

;; The complexity of a single code (like 029A) is equal to the result of multiplying these two values:

;; The length of the shortest sequence of button presses you need to type on your directional keypad in order to cause the code to be typed on the numeric keypad; for 029A, this would be 68.
;; The numeric part of the code (ignoring leading zeroes); for 029A, this would be 29.
;; In the above example, complexity of the five codes can be found by calculating 68 * 29, 60 * 980, 68 * 179, 64 * 456, and 64 * 379. Adding these together produces 126384.

;; Find the fewest number of button presses you'll need to perform in order to cause the robot in front of the door to type each code. What is the sum of the complexities of the five codes on your list?

(declaim (optimize (speed 3) (safety 0)))

(deftype posn () '(cons fixnum fixnum))

(defvar keypad
  (fset:map (#\7 (cons 0 0))
            (#\8 (cons 0 1))
            (#\9 (cons 0 2))
            (#\4 (cons 1 0))
            (#\5 (cons 1 1))
            (#\6 (cons 1 2))
            (#\1 (cons 2 0))
            (#\2 (cons 2 1))
            (#\3 (cons 2 2))
            (#\0 (cons 3 1))
            (#\A (cons 3 2))))

(defvar directions
  (fset:map (#\^ (cons 0 1))
            (#\A (cons 0 2))
            (#\< (cons 1 0))
            (#\V (cons 1 1))
            (#\> (cons 1 2))))

(defun map-values (map)
  (let ((s (fset:empty-set)))
    (fset:do-map (k v map)
      (declare (ignore k))
      (fset:includef s v))
    s))

(defun directions (from to key)
  (labels ((path-to-moves (path)
             (flet ((pos- (p2 p1)
                      (declare (type posn p1 p2))
                      (destructuring-bind (x2 . y2) p2
                        (destructuring-bind (x1 . y1) p1
                          (cons (- x2 x1) (- y2 y1)))))
                    (move (dir)
                      (cond ((equal dir '(0 . 1)) #\>)
                            ((equal dir '(0 . -1)) #\<)
                            ((equal dir '(1 . 0)) #\V)
                            ((equal dir '(-1 . 0)) #\^))))
               (let ((s (make-string (length path))))
                 (loop for i from 0
                       for p1 in path
                       for p2 in (cdr path)
                       do (setf (char s i) (move (pos- p2 p1)))
                       finally (setf (char s i) #\A))
                 s)))
           (move-line (from to)
             (declare (type posn from to))
             (destructuring-bind (fx . fy) from
               (destructuring-bind (tx . ty) to
                 (loop with dx = (signum (- tx fx)) and dy = (signum (- ty fy))
                       for x = fx then (+ x dx)
                       for y = fy then (+ y dy)
                       collect (cons x y)
                       until (and (= x tx) (= y ty)))))))
    (let* ((start (fset:@ key from))
           (end (fset:@ key to))
           (points (map-values key))
           (paths (list
                   (append (move-line start (cons (car start) (cdr end)))
                           (cdr (move-line (cons (car start) (cdr end)) end)))
                   (append (move-line start (cons (car end) (cdr start)))
                           (cdr (move-line (cons (car end) (cdr start)) end)))))
           (valid-paths (remove-if-not
                         (lambda (path)
                           (every (lambda (p) (fset:member? p points)) path))
                         (remove-duplicates paths :test #'equal))))
      (mapcar #'path-to-moves valid-paths))))

(defun first-sequence (str)
  (let ((sl (cons #\A (coerce str 'list))))
    (loop with strs = '("")
          for c1 in sl
          for c2 in (cdr sl)
          for next-strs = (loop for str in strs
                                append (loop for moves in (directions c1 c2 keypad)
                                             collect (concatenate 'string str moves)))
          do (setf strs next-strs)
          finally (return strs))))

(defun mk-shortest-sequence ()
  (let ((memo (fset:empty-map)) (chars '(#\A #\< #\V #\> #\^)))
    ;; Initialize the cache with the base cases
    (dolist (start chars)
      (dolist (end chars)
        (fset:includef memo (list start end 0)
                       (reduce #'min (mapcar #'length (directions start end directions))))))
    (labels ((shortest (c1 c2 depth)
               (or (fset:@ memo (list c1 c2 depth))
                   (let ((res (minimize-over (directions c1 c2 directions) depth)))
                     (fset:includef memo (list c1 c2 depth) res)
                     res)))
             (minimize-over (ss depth &optional (step-depth #'1-))
               (loop for s in ss
                     for sl = (cons #\A (coerce s 'list))
                     minimize
                     (loop for c1 in sl
                           for c2 in (cdr sl)
                           sum (shortest c1 c2 (funcall step-depth depth))))))
      (lambda (ss depth)
        (minimize-over ss depth #'identity)))))

;; --- Part Two ---
;; Just as the missing Historian is released, The Historians realize that a second member of their search party has also been missing this entire time!

;; A quick life-form scan reveals the Historian is also trapped in a locked area of the ship. Due to a variety of hazards, robots are once again dispatched, forming another chain of remote control keypads managing robotic-arm-wielding robots.

;; This time, many more robots are involved. In summary, there are the following keypads:

;; One directional keypad that you are using.
;; 25 directional keypads that robots are using.
;; One numeric keypad (on a door) that a robot is using.
;; The keypads form a chain, just like before: your directional keypad controls a robot which is typing on a directional keypad which controls a robot which is typing on a directional keypad... and so on, ending with the robot which is typing on the numeric keypad.

;; The door codes are the same this time around; only the number of robots and directional keypads has changed.

;; Find the fewest number of button presses you'll need to perform in order to cause the robot in front of the door to type each code. What is the sum of the complexities of the five codes on your list?

(defun main ()
  (flet ((solve (lines fn &key (iters 2))
           (reduce #'+ (mapcar (lambda (code)
                                 (* (parse-integer (subseq code 0 3))
                                    (funcall fn (first-sequence code) (1- iters))))
                               lines))))
    (let* ((lines (uiop:read-file-lines "input.data"))
           (fn (mk-shortest-sequence))
           (p1 (solve lines fn))
           (p2 (solve lines fn :iters 25)))
      (format t "Part 1: ~a~%" p1)
      (format t "Part 2: ~a~%" p2))))

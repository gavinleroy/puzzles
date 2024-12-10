(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '()))

(defpackage :day09
  (:use :cl))

(in-package :day09)

;; --- Day 9: Disk Fragmenter ---
;; Another push of the button leaves you in the familiar hallways of some friendly amphipods! Good thing you each somehow got your own personal mini submarine. The Historians jet away in search of the Chief, mostly by driving directly into walls.

;; While The Historians quickly figure out how to pilot these things, you notice an amphipod in the corner struggling with his computer. He's trying to make more contiguous free space by compacting all of the files, but his program isn't working; you offer to help.

;; He shows you the disk map (your puzzle input) he's already generated. For example:

;; 2333133121414131402
;; The disk map uses a dense format to represent the layout of files and free space on the disk. The digits alternate between indicating the length of a file and the length of free space.

;; So, a disk map like 12345 would represent a one-block file, two blocks of free space, a three-block file, four blocks of free space, and then a five-block file. A disk map like 90909 would represent three nine-block files in a row (with no free space between them).

;; Each file on disk also has an ID number based on the order of the files as they appear before they are rearranged, starting with ID 0. So, the disk map 12345 has three files: a one-block file with ID 0, a three-block file with ID 1, and a five-block file with ID 2. Using one character for each block where digits are the file ID and . is free space, the disk map 12345 represents these individual blocks:

;; 0..111....22222
;; The first example above, 2333133121414131402, represents these individual blocks:

;; 00...111...2...333.44.5555.6666.777.888899
;; The amphipod would like to move file blocks one at a time from the end of the disk to the leftmost free space block (until there are no gaps remaining between file blocks). For the disk map 12345, the process looks like this:

;; 0..111....22222
;; 02.111....2222.
;; 022111....222..
;; 0221112...22...
;; 02211122..2....
;; 022111222......
;; The first example requires a few more steps:

;; 00...111...2...333.44.5555.6666.777.888899
;; 009..111...2...333.44.5555.6666.777.88889.
;; 0099.111...2...333.44.5555.6666.777.8888..
;; 00998111...2...333.44.5555.6666.777.888...
;; 009981118..2...333.44.5555.6666.777.88....
;; 0099811188.2...333.44.5555.6666.777.8.....
;; 009981118882...333.44.5555.6666.777.......
;; 0099811188827..333.44.5555.6666.77........
;; 00998111888277.333.44.5555.6666.7.........
;; 009981118882777333.44.5555.6666...........
;; 009981118882777333644.5555.666............
;; 00998111888277733364465555.66.............
;; 0099811188827773336446555566..............
;; The final step of this file-compacting process is to update the filesystem checksum. To calculate the checksum, add up the result of multiplying each of these blocks' position with the file ID number it contains. The leftmost block is in position 0. If a block contains free space, skip it instead.

;; Continuing the first example, the first few blocks' position multiplied by its file ID number are 0 * 0 = 0, 1 * 0 = 0, 2 * 9 = 18, 3 * 9 = 27, 4 * 8 = 32, and so on. In this example, the checksum is the sum of these, 1928.

;; Compact the amphipod's hard drive using the process he requested. What is the resulting filesystem checksum? (Be careful copy/pasting the input for this puzzle; it is a single, very long line.)

(defun read-input (filename)
  (let ((s (with-open-file (stream filename)
             (read-line stream)))
        (files)
        (spaces))
    (loop for i from 0
          for c across s
          if (evenp i)
            do (push (parse-integer (string c)) files)
          else
            do (push (parse-integer (string c)) spaces)
          finally (return (values (nreverse files)
                                  (nreverse spaces))))))

;; HACK: don't read me pls
(defun part-1 ()
  (multiple-value-bind (files spaces)
      (read-input "input.data")
    (let ((files (coerce files 'vector))
          (spaces (coerce spaces 'vector))
          (res 0))
      (labels ((res+= (pos id)
                 (incf res (* pos id)))
               (run (pos fi fe sp mode)
                 (unless (and (< -1 fi (length files))
                              (< -1 fe (length files))
                              (< -1 sp (length spaces)))
                   (return-from run))
                 (cond
                   ((eq mode 'file)
                    (let ((n (aref files fi)))
                      (dotimes (n n)
                        (res+= (+ pos n) fi))
                      (setf (aref files fi) 0)
                      (run (+ pos n) (1+ fi) fe sp 'space)))
                   ((eq mode 'space)
                    (let ((n (aref spaces sp))
                          (c 0))
                      (loop for nn = n then nn
                            while (and (< 0 (- n c)) (<= fi fe))
                            if (zerop (aref files fe))
                              do (decf fe)
                            else
                              do (res+= (+ pos c) fe)
                              and do (decf (aref files fe))
                              and do (incf c))
                      (run (+ pos n) fi fe (1+ sp) 'file))))))
        (run 0 0 (1- (length files)) 0 'file)
        res))))

(defun read-input-2 (filename)
  (let ((s (with-open-file (stream filename)
             (read-line stream))))
    (loop for i from 0
          for c across s
          if (evenp i)
            collect (mk-file (/ i 2) (parse-integer (string c)))
          else
            collect (mk-space (parse-integer (string c))))))

(defun mk-space (size)
  (cons 'space size))

(defun mk-file (id size)
  (list 'file id size))

(defun spacep (node)
  (eq (car node) 'space))

(defun filep (node)
  (eq (car node) 'file))

(defun space-size (node)
  (when (spacep node)
    (cdr node)))

(defun file-id (node)
  (when (filep node)
    (cadr node)))

(defun file-size (node)
  (when (filep node)
    (caddr node)))

(defun size (n)
  (if (spacep n)
      (space-size n)
      (file-size n)))

;; HACK: don't read me pls
(defun part-2 ()
  (let ((disk (read-input-2 "input.data")))
    (labels ((clear-file (nd ls)
               (loop for cnd on ls
                     when (equal nd (car cnd))
                       do (setf (car cnd) (mk-space (file-size nd)))
                       and do (return)))
             (compact (remaining)
               (unless (null remaining)
                 (block loopy
                   (loop for nd = (car remaining)
                         for cns on disk
                         for (n . rest) = cns
                         when (equal nd n)
                           do (return-from loopy)
                         when (and (spacep n)
                                   (<= (file-size nd) (space-size n)))
                           do (clear-file nd rest)
                           and do (setf (car cns) nd)
                           and do (let ((nxt-size (- (space-size n) (file-size nd))))
                                    (when (< 0 nxt-size)
                                      (setf (cdr cns) (cons (mk-space nxt-size) rest))))
                           and do (return-from loopy)))
                 (compact (cdr remaining))))
             (hashit (disk)
               (loop for i = 0 then (+ i (size n))
                     for (n . rest) on disk
                     when (filep n)
                       sum (loop for j from i below (+ i (file-size n))
                                 sum (* j (file-id n))))))
      (compact (reverse (remove-if-not #'filep disk)))
      (hashit disk))))

(defun main ()
  (format t "Part 1: ~a~%" (part-1))
  (format t "Part 2: ~a~%" (part-2)))


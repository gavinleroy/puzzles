(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fset :str)))

(defpackage :day23
  (:use :cl))

(in-package :day23)

;; --- Day 23: LAN Party ---
;; As The Historians wander around a secure area at Easter Bunny HQ, you come across posters for a LAN party scheduled for today! Maybe you can find it; you connect to a nearby datalink port and download a map of the local network (your puzzle input).

;; The network map provides a list of every connection between two computers. For example:

;; kh-tc
;; qp-kh
;; de-cg
;; ka-co
;; yn-aq
;; qp-ub
;; cg-tb
;; vc-aq
;; tb-ka
;; wh-tc
;; yn-cg
;; kh-ub
;; ta-co
;; de-co
;; tc-td
;; tb-wq
;; wh-td
;; ta-ka
;; td-qp
;; aq-cg
;; wq-ub
;; ub-vc
;; de-ta
;; wq-aq
;; wq-vc
;; wh-yn
;; ka-de
;; kh-ta
;; co-tc
;; wh-qp
;; tb-vc
;; td-yn
;; Each line of text in the network map represents a single connection; the line kh-tc represents a connection between the computer named kh and the computer named tc. Connections aren't directional; tc-kh would mean exactly the same thing.

;; LAN parties typically involve multiplayer games, so maybe you can locate it by finding groups of connected computers. Start by looking for sets of three computers where each computer in the set is connected to the other two computers.

;; In this example, there are 12 such sets of three inter-connected computers:

;; aq,cg,yn
;; aq,vc,wq
;; co,de,ka
;; co,de,ta
;; co,ka,ta
;; de,ka,ta
;; kh,qp,ub
;; qp,td,wh
;; tb,vc,wq
;; tc,td,wh
;; td,wh,yn
;; ub,vc,wq
;; If the Chief Historian is here, and he's at the LAN party, it would be best to know that right away. You're pretty sure his computer's name starts with t, so consider only sets of three computers where at least one computer's name starts with t. That narrows the list down to 7 sets of three inter-connected computers:

;; co,de,ta
;; co,ka,ta
;; de,ka,ta
;; qp,td,wh
;; tb,vc,wq
;; tc,td,wh
;; td,wh,yn
;; Find all the sets of three inter-connected computers. How many contain at least one computer with a name that starts with t?

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun read-input (filename)
  (macrolet ((set++ (es from to)
               `(fset:includef ,es ,from
                 (fset:with (fset:@ ,es ,from) ,to))))
    (loop with edges = (fset:empty-map (fset:empty-set))
          for line in (uiop:read-file-lines filename)
          for (from to) = (str:split "-" line)
          do (set++ edges from to)
             (set++ edges to from)
          finally (return edges))))

;; FROM WIKIPEDIA
;; algorithm BronKerbosch1(R, P, X) is
;;     if P and X are both empty then
;;         report R as a maximal clique
;;     for each vertex v in P do
;;         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
;;         P := P \ {v}
;;         X := X ⋃ {v}
(defun k-cliques (edges &key (test (lambda (_) (declare (ignore _)) t)))
  (labels ((cliques (R P X)
             (when (and (fset:empty? P) (fset:empty? X) (funcall test R))
               (return-from cliques (list R)))
             (loop until (fset:empty? P)
                   for v = (fset:arb P)
                   for nodes = (fset:@ edges v)
                   append (cliques (fset:with R v) (fset:intersection P nodes)
                                   (fset:intersection X nodes))
                     into clqs
                   do (fset:removef P v)
                      (fset:includef X v)
                   finally (return clqs))))
    (cliques (fset:empty-set) (fset:domain edges) (fset:empty-set))))

(defun chief-set-p (set)
  (fset:do-set (vertex set)
    (when (char= #\t (char vertex 0))
      (return-from chief-set-p t))))

(defun choose-3 (set test)
  (let ((ls (fset:empty-set)))
    (fset:do-set (a set)
      (fset:do-set (b set)
        (fset:do-set (c set)
          (let ((s (fset:set a b c)))
            (when (and (= 3 (fset:size s)) (funcall test s))
              (fset:includef ls s))))))
    ls))

(defun part-1 (all-cliques)
  (fset:size
   (fset:reduce
    (lambda (acc c) (fset:union acc (choose-3 c #'chief-set-p)))
    (fset:filter #'chief-set-p all-cliques)
    :initial-value (fset:empty-set))))

;; --- Part Two ---
;; There are still way too many results to go through them all. You'll have to find the LAN party another way and go there yourself.

;; Since it doesn't seem like any employees are around, you figure they must all be at the LAN party. If that's true, the LAN party will be the largest set of computers that are all connected to each other. That is, for each computer at the LAN party, that computer will have a connection to every other computer at the LAN party.

;; In the above example, the largest set of computers that are all connected to each other is made up of co, de, ka, and ta. Each computer in this set has a connection to every other computer in the set:

;; ka-co
;; ta-co
;; de-co
;; ta-ka
;; de-ta
;; ka-de
;; The LAN party posters say that the password to get into the LAN party is the name of every computer at the LAN party, sorted alphabetically, then joined together with commas. (The people running the LAN party are clearly a bunch of nerds.) In this example, the password would be co,de,ka,ta.

;; What is the password to get into the LAN party?

(defun part-2 (all-cliques)
  (fset:convert
   'list
   (fset:first
    (fset:sort
     all-cliques
     #'> :key #'fset:size))))

(defun main ()
  (let* ((edges (read-input "input.data"))
         (cliques (k-cliques edges)))
    (format t "Part 1: ~d~%" (part-1 cliques))
    (format t "Part 2: ~{~A~^,~}~%" (part-2 cliques))))

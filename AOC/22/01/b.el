;;; a.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 gavinleroy
;;
;; Author: gavinleroy <gavinleroy6@gmail.com>
;; Maintainer: gavinleroy <gavinleroy6@gmail.com>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gavinleroy/a
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'seq)
(require 'f)

(defvar *test-input*
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"
  )

(defun solve+ (input)
  (let* ((calories (mapcar
                    (lambda (carrying)
                      (seq-reduce #'+ carrying 0)) input))
         (srtd (sort calories #'>)))
    (seq-reduce #'+ (seq-subseq srtd 0 3) 0)))

(defun parse-input-string (input)
  (mapcar
   (lambda (s)
     (mapcar #'string-to-number
             (split-string s "\n")))
   (split-string input "\n\n")))

(defun b-solve (fn)
  (interactive "input file:")
  (let ((answer
         (solve+
          (parse-input-string
           (f-read-text fn)))))
    (message "%d" answer)))

(provide 'b)
;;; a.el ends here

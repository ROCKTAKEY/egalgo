;;; egalgo.el --- Genetic algorithm for Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: data

;; Version: 0.0.0

;; Package-Requires: ((dash "2.14"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'dash)

(defmacro egalgo--crossover (index gene1 gene2)
  "Crossover GENE1 and GENE2 on INDEXth gap.
Do not use 0 (and less) as INDEX. First gap is indexed 1."
  (declare (debug (integerp listp listp)))
  (let ((temp (cl-gensym)))
    `(progn
       (when (<= ,index 0)
        (error "You cannot use 0 or less number as index."))
       (setq ,temp (nthcdr ,index ,gene1))
       (setcdr (nthcdr (1- ,index) ,gene1) (nthcdr ,index ,gene2))
       (setcdr (nthcdr (1- ,index) ,gene2) ,temp)
       (list ,gene1 ,gene2))))

(defun egalgo--rand-bool (probability)
  "Return t with probability of PROBABILITY."
  (< (cl-random 1.0) probability))

(defun egalgo--t-p (object)
  ""
  (eq t object))

(defvar egalgo--generate-alist
  '((vectorp  . egalgo--generate-range)
    (listp    . egalgo--generate-choose)
    (egalgo--t-p    . egalgo--generate-bool)
    (integerp . egalgo--generate-choose-from-zero)))

(defun egalgo--generate-range (arg)
  ""
  (let* ((bgn (float (aref arg 0)))
         (end (float (aref arg 1)))
         (range (- end bgn)))
    (+ (cl-random range) bgn)))

(defun egalgo--generate-choose (arg)
  ""
  (nth (cl-random (length arg)) arg))

(defun egalgo--generate-bool (_arg)
  ""
  (egalgo--rand-bool 0.5))

(defun egalgo--generate-choose-from-zero (arg)
  ""
  (cl-random arg))

(provide 'egalgo)
;;; egalgo.el ends here

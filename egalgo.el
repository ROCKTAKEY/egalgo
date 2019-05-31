;;; egalgo.el --- Genetic algorithm for Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: data

;; Version: 0.0.0

;; Package-Requires:

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
(provide 'egalgo)
;;; egalgo.el ends here

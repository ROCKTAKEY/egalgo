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

;; Some functions

(defsubst egalgo--rand-bool (probability)
  "Return t with probability of PROBABILITY."
  (< (cl-random 1.0) probability))

(defun egalgo--t-p (object)
  ""
  (eq t object))


;; Selectors
;; Selector should not select chromosome rated nil.

(defvar egalgo-selector-alias
  '((roulette . egalgo-roulette-selector)))

(defun egalgo-roulette-selector (rates)
  "Select 1 chromosome by roulette from RATES.  Return index.
RATES are list of rate of each chromosome, or nil (unselectable)."
  (let* ((temp (if (car rates) (car rates) 0))
         (r-sum
          (--map (setq temp (+ (if it it 0) temp))
                 rates))
         (sum (car (last r-sum)))
         (rand (cl-random (float sum)))
         (i 0))
    (--each-while r-sum (< it rand) (setq i (1+ i)))
    i))

(defun egalgo--select-2 (rates selector)
  "Select 2 chromosomes with roulette using RATES.
Return list of 2 indexes."
  (let* ((first  (funcall selector rates))
         (temp (nth first rates))
         second)
    ;; Selected index is temporarily replaced nil (means unselectable).
    (setf (nth first rates) nil)

    (setq second (funcall selector rates))

    (setf (nth first rates) temp)
    (list first second)))


;; Crossover

(defmacro egalgo--crossover (index chromosome1 chromosome2)
  "Crossover CHROMOSOME1 and CHROMOSOME2 on INDEXth gap.
Do not use 0 (and less) as INDEX. First gap is indexed 1."
  (declare (debug (form place place)))
  (let ((temp (cl-gensym))
        (i (cl-gensym)))
    `(let ((,i ,index))
       (when (<= ,i 0)
        (error "You cannot use 0 or less number as index."))
       (setq ,temp (nthcdr ,i ,chromosome1))
       (setcdr (nthcdr (1- ,i) ,chromosome1) (nthcdr ,i ,chromosome2))
       (setcdr (nthcdr (1- ,i) ,chromosome2) ,temp)
       (list ,chromosome1 ,chromosome2))))


;; Generate nucleotides and a chromosome.

(defvar egalgo--generate-alist
  '((vectorp  . egalgo--generate-range)
    (listp    . egalgo--generate-choose)
    (egalgo--t-p    . egalgo--generate-bool)
    (integerp . egalgo--generate-choose-from-zero-to)))

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

(defun egalgo--generate-choose-from-zero-to (arg)
  ""
  (cl-random arg))

(defun egalgo--generate-chromosome-forms (chromosome-definition)
  ""
  (vconcat
   (--map `(,(cdr (-first
                   (lambda (arg) (funcall (car arg) it))
                   egalgo--generate-alist))
            ',it)
          chromosome-definition)))

(defun egalgo--generate-chromosomes-from-forms (chromosome-forms size)
  ""
  (let (result)
    (--dotimes size
      (push
       (map 'list 'eval chromosome-forms)
       result))
    result))

(defun egalgo--generate-chromosomes-from-definition (chromosome-definition size)
  ""
  (egalgo--generate-chromosomes-from-forms
   (egalgo--generate-chromosome-forms chromosome-definition) size))


;; For users.

(provide 'egalgo)
;;; egalgo.el ends here

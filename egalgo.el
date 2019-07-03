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


;; Generate genes and a chromosome.

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

(cl-defun egalgo-run (chromosome-definition
                rater            ;Function returning non-negative float
                &key
                (size 100)       ;non-negative integer
                (crossover 0.9)  ;[0, 1]
                (mutation 0.01)  ;[0, 1]
                (n-point-crossover 1)    ;t ... uniformcrossover
                (selector 'roulette)     ;function or alias
                (termination 1000)       ;integer: generation number.
                ;; arguments showed below are available in the future.
                (elite 0)                ;non-negative integer
                (log nil)                ;bool
                (async nil))             ;bool
  ""
  (let* ((chromosome-forms
          (egalgo--generate-chromosome-forms chromosome-definition))
         (chromosomes
          (egalgo--generate-chromosomes-from-forms chromosome-forms size))
         (selector (or (cdr (assq selector egalgo-selector-alias))
                       selector))
         (length (length chromosome-definition))
         (generation 0)
         i next-chromosomes selected-indexes old-rates rates)
    (while
        (cl-case termination
          (t
           (if (integerp termination)
               (<= generation termination)
             (error "Wrong type of argument."))))

      (setq rates (-map rater chromosomes))

      ;; Counter which has length of `next-chromosomes'.
      (setq i 0)

      (while (< i size)
        (if (and (< 1 (- size i))
                 (egalgo--rand-bool crossover))

            ;; Crossover
            (let (selected1 selected2)
              ;; Select 2 chromosomes which will be crossovered.
              (setq selected-indexes (egalgo--select-2 rates selector))
              (setq selected1 (copy-list
                               (nth (car  selected-indexes) chromosomes)))
              (setq selected2 (copy-list
                               (nth (cadr selected-indexes) chromosomes)))

              ;; Crossover chromosomes.
              (if (eq n-point-crossover t)
                  ;; uniformcrossover.
                  (cl-dotimes (n (1- length))
                    (when (egalgo--rand-bool 0.5)
                      (egalgo--crossover (1+ n) selected1 selected2)))
                ;; `n-point-crossover' point crossover.
                (cl-dotimes (n n-point-crossover)
                  (egalgo--crossover
                   (1+ (cl-random (1- length)))
                   selected1 selected2)))

              ;; Push the 2 crossovered chromosomes to next-chromosomes.
              (push selected1 next-chromosomes)
              (push selected2 next-chromosomes)
              (setq i (+ i 2)))

          ;; Push selected chromosome to the next-chromosomes.
          (push (copy-list (nth (funcall selector rates) chromosomes))
                next-chromosomes)
          (setq i (1+ i))))

      ;; Mutation
      (let (new-ncl now)
        (dolist (c next-chromosomes)
          (dotimes (n length)
            (when (egalgo--rand-bool mutation)
              (setq now (nth n c))
              (while (eq now
                         (setq new-ncl (eval (aref chromosome-forms n)))))
              (setf (nth n c) new-ncl)))))

      ;; Save
      (setq old-rates rates)
      (setq chromosomes next-chromosomes)

      (setq next-chromosomes nil)
      (setq generation (1+ generation))

      (message "generation: %d / Max rate: %f / Average rate: %f\n%s"
               generation (-max rates)
               (/ (-sum rates) size)
               (prin1-to-string rates)))

    (list :chromosomes chromosomes
          :rates rates)))

(provide 'egalgo)
;;; egalgo.el ends here

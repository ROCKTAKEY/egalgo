;;; egalgo-test.el --- Test for egalgo

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

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
(require 'ert)

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'egalgo)

(ert-deftest egalgo--crossover ()
  (should
   (equal
    (list '( 1  2  3 14 15 16 17 18 19 20)
          '(11 12 13  4  5  6  7  8  9 10))
    (let ((a (list  1  2  3  4  5  6  7  8  9 10))
          (b (list 11 12 13 14 15 16 17 18 19 20)))
     (egalgo--crossover 3 a b)
     (list a b))))
  (should-error
   (egalgo--crossover 0 '(1 2 3) '(4 5 6)))
  (should-error
   (egalgo--crossover -10 '(1 2 3) '(4 5 6))))

(ert-deftest egalgo--generate-chromosomes ()
  (let ((chromosomes (egalgo--generate-chromosomes-from-definition
                '(1 2 3 [10 20] t) 100)))
    (dolist (chromosome chromosomes)
     (should
      (equal (nth 0 chromosome) 0))
     (should
      (or
       (equal (nth 1 chromosome) 0)
       (equal (nth 1 chromosome) 1)))
     (should
      (or
       (equal (nth 2 chromosome) 0)
       (equal (nth 2 chromosome) 1)
       (equal (nth 2 chromosome) 2)))
     (should
      (and (< 10 (nth 3 chromosome))
           (< (nth 3 chromosome) 20)))
     (should
      (or (eq (nth 4 chromosome) nil)
          (eq (nth 4 chromosome) t))))))

(ert-deftest egalgo-roulette-selector ()
  (let* ((rates '(1 4 7))
         (selected (egalgo-roulette-selector rates)))
    (should
     (or (equal selected 0)
         (equal selected 1)
         (equal selected 2)))))

(ert-deftest egalgo--select-2 ()
  (let* ((lst (egalgo--select-2 '(1 2 4 69) 'egalgo-roulette-selector))
         (first (car lst))
         (second (cadr lst)))
    (dolist (it lst)
      (should (or (equal it 0)
                  (equal it 1)
                  (equal it 2)
                  (equal it 3))))
    (should-not (equal first second))))

(ert-deftest egalgo-run ()
  (let ((goods
         ;; (Volume . Value)
         '((3 . 4)
           (2 . 1)
           (6 . 3)
           (6 . 2)
           (2 . 6)
           (7 . 5)
           (5 . 5)
           (5 . 9)
           (2 . 3)
           (4 . 5)))
        (knapsack-size 10))
    (cl-flet
        ((knapsack-rater
          (chromosome)
          (let ((value 0)
                (volume 0))
            (dotimes (i (length chromosome))
              (when (nth i chromosome)
                (setq volume (+ volume (car (nth i goods))))
                (setq value  (+ value  (cdr (nth i goods))))))
            (if (<= volume knapsack-size)
                value
              0))))

      ;; Minimum running.
      (egalgo-run
       '(t t t t t t t t t t)                 ;t means boolean gene.
       #'knapsack-rater)

      ;; Additional running.
      (egalgo-run
       '(t t t t t t t t t t)                 ;t means boolean gene.
       #'knapsack-rater
       :size 50             ;Size of each generation.
       :crossover 0.8       ;Probability of doing crossover.
       :mutation 0.05       ;Probability of each gene mutating.
       :n-point-crossover 2 ;means 2 point crossover. `t' means unicrossover.
       :termination 200)    ;Number of maximum generation.

      (egalgo-run
       '(t t t t t t t t t t)                 ;t means boolean gene.
       #'knapsack-rater
       :termination 100
       :size 30
       :elite 1                               ;keep 1 elite chromossomes
       :mutation 0.01
       :n-point-crossover t)                  ;means unicrossover

      (egalgo-run
       '(t t t t t t t t t t)
       #'knapsack-rater
       ;; termination can be function.
       :termination
       (lambda (stack-of-rates _generation)
         (or
          (not (nth 1 stack-of-rates))
          (< 10 (abs (- (-sum (nth 0 stack-of-rates))
                        (-sum (nth 1 stack-of-rates)))))))))))

(provide 'egalgo-test)
;;; egalgo-test.el ends here

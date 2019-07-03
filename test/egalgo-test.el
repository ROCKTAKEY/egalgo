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

(provide 'egalgo-test)
;;; egalgo-test.el ends here

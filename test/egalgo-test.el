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
(require 'egalgo)

(ert-deftest egalgo--crossover ()
  (should
   (equal
    (list '(1 2 3 14 15 16 17 18 19 20)
          '(11 12 13 4 5 6 7 8 9 10))
    (let ((a '(1 2 3 4 5 6 7 8 9 10))
         (b '(11 12 13 14 15 16 17 18 19 20)))
     (egalgo--crossover 3 a b)
     (list a b))))
  (should-error
   (egalgo--crossover 0 '(1 2 3) '(4 5 6)))
  (should-error
   (egalgo--crossover -10 '(1 2 3) '(4 5 6))))

(provide 'egalgo-test)
;;; egalgo-test.el ends here

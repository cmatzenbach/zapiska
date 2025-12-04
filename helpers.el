;;; helpers.el --- General helper functions used within package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Matzenbach

;; Author: Chris Matzenbach <matzy@proton.me>
;; Keywords: lisp

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
(defun find-key-for-value (plist target-value &optional test-function)
  "Finds the key in PLIST whose value matches TARGET-VALUE.
TEST-FUNCTION is an optional comparison function, defaulting to `equal`."
  (let ((current-plist plist)
        (test (or test-function 'equal)))
    (catch 'found
      (while (and current-plist (cdr current-plist))
        (let ((key (car current-plist))
              (value (cadr current-plist)))
          (when (funcall test value target-value)
            (throw 'found key)))
        (setq current-plist (cddr current-plist))))))

(defun decimal-to-percentage-string (decimal-num &optional decimal-places)
  "Convert a decimal number to a percentage string.
DECIMAL-NUM is a floating point number (e.g., 0.75).
DECIMAL-PLACES is an optional integer for rounding (e.g., 2 for 75.00%)."
  (let* ((percentage (* decimal-num 100.0))
         (format-string (if decimal-places
                            (format "%%.%df%%%%" decimal-places) ; e.g., "%.2f%%"
                          "%.2f%%")) ; default to 2 decimal places if not specified
         (result (format format-string percentage)))
    result))


(provide 'helpers)
;;; helpers.el ends here

;;; ob-spice.el --- Babel Functions for spice
;;; -*- coding: utf-8 -*-

;; License: GPL v3, or any later version
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating spice.

;;; Requirements:

;; - ngspice :: http://ngspice.sourceforge.net/

;;; Code:

(require 'ob)

;; (lambda (text) (setq body (concat text "\n" body)))
(defun org-babel-spice-vector-search (body vars)
  "Replace first instance in BODY for all VARS."
  (mapc (lambda (pair)
	  (if (string-match (format
			     "\\$%s\\[\\([0-9]\\)\\]"
			     (car pair))
			    body)
	      (let ((replacement (nth
				  (string-to-number (match-string 1 body))
				  (cadr pair))))
		(setq body(format "%s%s%s"
				  (substring body 0 (match-beginning
						     0))
				  replacement
				  (substring body (match-end 0)))))))
	vars)
  body
  )

(defun org-babel-expand-body:spice (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(prologue (cdr (assq :prologue params)))
	(epilogue (cdr (assq :epilogue params)))
	(file (cdr (assq :file params)))
	(old-body ""))
    ;; replace vector variables preceded by '$' and followed by the
    ;; index in square brackets starting at 0. Matches without
    ;; preceding or succeeding spaces.
    (while (not (string= old-body body))
      (setq old-body body)
      (setq body (org-babel-spice-vector-search body vars))
      )
    ;; replace any variable names preceded by '$' with the actual
    ;; value of the variable. Matches only with succeeding space or
    ;; end of line to prevent namespace limitations.
    (mapc (lambda (pair)
	    (setq body (replace-regexp-in-string
			(format "\\$%s\\( \\)\\|\\$%s$" (car pair)
				(car pair))
			(format "%s\1" (cdr pair))
			body)))
	  vars)
    ;; TODO :file stuff ....
    
    (when prologue (setq body (concat prologue "\n" body)))
    (when epilogue (setq body (concat body "\n" epilogue)))
    body))

(defun org-babel-execute:spice (body params)
  "Execute a block of Spice code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let ((body (org-babel-expand-body:spice body params))
	(gnuplot (cdr (assq :gnuplot params))))

    ;; TODO deal with temporary files

    (org-babel-eval "ngspice -b " body)


    ;; TODO read outputs from files
    
    ;; TODO gnuplot options
    (if (string= "yes" gnuplot)
	nil ;return content(!) of gnuplot.plt for :post processing or
					;nowebbing spice into gnuplot
      nil ;return normal spice output
      )
    ))


(provide 'ob-spice)
;;; ob-spice.el ends here

;;; ob-spice.el --- Babel Functions for spice

;; Author: Ferdinand Pieper <mail@pie.tf>
;; Keywords: literate programming, reproducible research

;; Copyright (c) 2018-2020 Ferdinand Pieper

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

;; Org-Babel support for evaluating spice simulations.

;;; Requirements:

;; - ngspice :: http://ngspice.sourceforge.net/

;;; Code:

(require 'ob)

(defvar org-babel-spice-eoe-indicator ":org_babel_spice_eoe"
  "String to indicate that evaluation has completed.")
(defvar org-babel-spice-command "ngspice"
  "Name of command to use for executing ngspice.")

(defun org-babel-spice-initiate-session (&optional session dir _params)
  "Initiate a ngspice session.
Create comint buffer SESSION running ngspice starting in
`default-directory' or DIR if specified."
  (let* ((sessionname (if (or (not session) (string= session "none"))
                          "spice" session))
         (session (make-comint sessionname org-babel-spice-command)))
    (if (and dir (file-name-absolute-p dir))
	;; absolute dir
	(comint-simple-send session (format "cd '%s'" dir))
      ;; relative dir
      (comint-simple-send session (format "cd '%s'" default-directory)))
    session))

;;;; Helper functions

(defun org-babel-variable-assignments:spice (params)
  "Return a list of spice statements to set the variables in PARAMS."
  (mapcar
   (lambda (pair)
     (format "set %s=%s"
             (car pair)
             (org-babel-spice-var-to-spice (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-spice-var-to-spice (var)
  "Convert VAR into a spice variable."
  (if (listp var)
      (concat "( " (mapconcat #'org-babel-spice-var-to-spice var " ") " )")
    (format "%S" var)))

(defun org-babel-spice-vector-search (body vars)
  "Replace first instance in BODY for all VARS."
  (mapc (lambda (pair)
          (if (string-match (format
                             "\\$%s\\[\\([0-9]\\)\\]"
                             (car pair))
                            body)
              (let ((replacement (nth
                                  (string-to-number (match-string 1 body))
                                  pair)))
                (setq body(format "%s%s%s"
                                  (substring body 0 (match-beginning
                                                     0))
                                  replacement
                                  (substring body (match-end 0)))))))
        vars)
  body)

(defun org-babel-spice-replace-vars (body vars)
  "Expand BODY according to VARS."
  (let ((old-body ""))
    ;; replace vector variables preceded by '$' and followed by the
    ;; index in square brackets starting at 0. Matches without
    ;; preceding or succeeding spaces.
    (while (not (string= old-body body))
      (setq old-body body)
      (setq body (org-babel-spice-vector-search body vars))
      )
    ;; replace any variable names preceded by '$' with the actual
    ;; value of the variable. Matches only with succeeding space, dot
    ;; or end of line to prevent namespace limitations.
    (mapc (lambda (pair)
            (setq body (replace-regexp-in-string
                        (format "\\$%s\\([ \.]\\)\\|\\$%s$" (car pair)
                                (car pair))
                        (format "%s\\1" (cdr pair))
                        body)))
          vars)
    body))

(defun org-babel-expand-body:spice (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
        (prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (setq body (org-babel-spice-replace-vars body vars))
    ;; add prologue/epilogue
    (when prologue (setq body (concat prologue "\n" body)))
    (when epilogue (setq body (concat body "\n" epilogue)))
    body))

(defun org-babel-spice-trim-body (body)
  "Prepare BODY to be used in interactive ngspice session."
  ;; random control codes after $var inserts
  (replace-regexp-in-string
   "" " "
   ;; .control .endc lines
   (replace-regexp-in-string
    "^ *\\.\\(control\\|endc\\) *$" ""
    ;; comment lines
    (replace-regexp-in-string
     "^ *\\*.*$" "" body))))

(defun org-babel-execute:spice (body params)
  "Execute a block of Spice code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* (;(body (org-babel-expand-body:spice body params))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (session (org-babel-spice-initiate-session
                   (cdr (assq :session params))
                   (cdr (assq :dir params))))
         (vars (org-babel--get-vars params))
         (break-index (if (string-match "^ *\.end *$" body)
                          (match-end 0) 0))
         ;;vars need to be replaced as they don't work when using source
         (circuit-body (org-babel-expand-body:spice
                        (substring body 0 break-index)
                        (assq-delete-all :epilogue (copy-alist params))))
         ;; vars can be managed using set
	 ;; just remember to use $file\.txt instead of $file.txt
         (control-body (org-babel-spice-trim-body (substring body break-index)))
         (full-control-body (if (not (string= control-body ""))
				(org-babel-expand-body:generic
				 control-body
				 (assq-delete-all :prologue (copy-alist params))
				 (org-babel-variable-assignments:spice params)) ""))
         (circuit-file (if circuit-body (org-babel-temp-file "spice-body-" ".cir")))
         (result))

    ;; Source circuit-body
    (with-temp-file circuit-file (insert circuit-body))
    ;; Evaluate
    (org-babel-spice-evaluate session full-control-body
			      result-type circuit-file result-params)))

(defun org-babel-spice-evaluate (buffer body result-type &optional file result-params)
  "Use ngspice process in BUFFER to eval BODY and return results.
If RESULT-TYPE equals `output' return all outputs, if it equals
`value' return only value of last statement. FILE can refer to a
spice input file that is sourced before BODY execution is
started."
  (let ((eoe-string (format "echo \"%s\"" org-babel-spice-eoe-indicator))
        (eval-body (if file (concat "source " file "\n" body) "")))
    (pcase result-type
      (`output
       ;; Force session to be ready
       ;;(org-babel-comint-with-output
       ;;    (buffer org-babel-spice-eoe-indicator t eoe-string)
       ;;  (insert eoe-string) (comint-send-input nil t))
       ;; Eval body
       (org-babel-chomp
	(replace-regexp-in-string
	 "^Current directory: .*\n" ""
	 (replace-regexp-in-string
	  "^\\(ngspice [0-9]+ -> *\n*\\)*" ""
	  (mapconcat
	   #'identity
	   (butlast
	    (cdr
	     (split-string
	      (mapconcat
	       #'org-trim
	       (org-babel-comint-with-output (buffer org-babel-spice-eoe-indicator t eval-body)
		 (mapc (lambda (line)
			 (insert (org-babel-chomp line)) (comint-send-input nil t))
		       (list eval-body
			     eoe-string
			     "\n")))
	       "\n") "[\r\n]")) 2) "\n"))))
       )
      (`value
       (let ((tmp-file (org-babel-temp-file "spice-")))
	 (org-babel-comint-with-output
	     (buffer org-babel-spice-eoe-indicator t eval-body)
	   (mapc
	    (lambda (line)
	      (insert (org-babel-chomp line)) (comint-send-input nil t))
	    (append (list eval-body)
		    (list (format "echo !! > %s" tmp-file)
			  (format "echo \"%s\"" org-babel-spice-eoe-indicator)
			  )))
	   (comint-send-input nil t))
	 ;; split result to output multiple comma separated vars as table
	 (let* ((raw-result (org-babel-chomp (org-babel-eval-read-file tmp-file)))
		(result nil))
	   (when (string-match "^print" raw-result)
	     (let ((tmp-file (org-babel-temp-file "spice-")))
	       (org-babel-comint-with-output
		   (buffer org-babel-spice-eoe-indicator)
		 (mapc
		  (lambda (line)
		    (insert (org-babel-chomp line)) (comint-send-input nil t))
		  (append (list (format "%s > %s" raw-result tmp-file))
			  (list (format "echo \"%s\"" org-babel-spice-eoe-indicator))))
		 (comint-send-input nil t))
	       (setq raw-result (org-babel-eval-read-file tmp-file))))
	   (setq result (org-babel-spice-cleanup-result
			 (org-babel-chomp raw-result)))
	   (if (or (not (listp result)) (cdr result))
	       result
	     (car result))
	   )))
      ;;todo: add "smart" result type to display measurements (or echos?) & plot filenames
      )))

(defun org-babel-spice-cleanup-result (result)
  "Cleanup value to return instead of RESULT.
Commands that write to files return the filename."
  (let* ((index (if (string-match "^ *[^ ]*" result)
		    (match-end 0) 0))
	 (type (substring result 0 index))
	 (arg (replace-regexp-in-string "^ *[^ ]* \\([^ ]*\\).*" "\\1" result)))
    (pcase type
      ((or "wrdata" "write" "hardcopy") arg)
      ("gnuplot" (format "%s.png" arg))
      ("echo" (split-string (substring result (+ index 1)) ","))
      (_ result))))

(provide 'ob-spice)
;;; ob-spice.el ends here

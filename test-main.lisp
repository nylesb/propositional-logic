(load "main.lisp")

;;; Perform tests for wfp-checker function
(defparameter file (open "test-inputs/wfp-checker-tests.txt"))
(princ (format nil "~%~%----------Test results for wfp-checker----------~%~%"))
(do ((eof nil (not (listen file))))
  (eof)
  (let ((test-case (read-line file)))
    (princ (format nil "Working on ~A~%  " test-case))
    (princ (wfp-checker test-case))))

;;; Perform tests for TruthValue function
(defparameter file (open "test-inputs/TruthValue-tests.txt"))
(princ (format nil "~%~%----------Test results for TruthValue----------~%~%"))
(do ((eof nil (not (listen file))))
  (eof)
  (let ((test-values (read file))
        (test-proposition (read file)))
    (princ (format nil "Working on ~S, ~S~%  " test-values test-proposition))
    (princ (TruthValue test-values test-proposition))))
;; Verify that quote notation does work.  Had trouble with reading in things like
;; '((P t)) and then passing that read value into the function
(print (format nil "Verify that quoted inputs works properly:"))
(print (TruthValue '((P t) (Q nil)) '(OR (P) (Q))))

;;; Perform tests for wfp-checkerFOL function
(princ (format nil "~%~%----------Test results for wfp-checkerFOL----------~%~%"))
(defparameter file (open "test-inputs/wfp-checkerFOL-tests.txt"))
(do ((eof nil (not (listen file))))
  (eof)
  (let ((test-case (read-line file)))
    (princ (format nil "Working on ~A~%  " test-case))
    (print (wfp-checker test-case))))
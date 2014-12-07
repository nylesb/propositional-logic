(load "main.lisp")

;;; Perform tests for wfp-checker function
(defparameter file (open "test-inputs/wfp-checker-tests.txt"))
(do ((eof nil (not (listen file))))
  (eof)
  (let ((test-case (read-line file)))
    (princ (format nil "Working on ~A~%  " test-case))
    (print (wfp-checker test-case))))

;;; Perform tests for TruthValue function
(defparameter file (open "test-inputs/TruthValue-tests.txt"))
(do ((eof nil (not (listen file))))
  (eof)
  (let ((test-values (read file))
        (test-proposition (read file)))
    (print test-values)
    (princ (format nil "Working on ~S, ~S~%  " test-values test-proposition))
    (print (TruthValue test-values test-proposition))))
;; Verify that quote notation does work.  Had trouble with reading in things like
;; '((P t)) and then passing that read value into the function
(print (format nil "Verify that quoted inputs works properly:"))
(print (TruthValue '((P t) (Q nil)) '(OR (P) (Q))))
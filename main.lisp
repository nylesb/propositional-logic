(defun wfp-checker (input)
  "Input is a string of the proposition to check.
  Returns t if input is a well-formed proposition, nil otherwise.
  (Since input a str, double quotes of the propsotion must be escaped, i.e. use \".)
  (AND/OR can take any number of arguments and be a wfp still.)"
  (labels ((verify-number (phrase)
            "Phrase is a list representing a proposition.  Recursively verifies each
            syntax word has proper num of arguments.  Returns nil if not."
            (let ((term (first phrase))
                  (rest-length (length (rest phrase))))
              (cond ((equal term 'NOT) (if (/= rest-length 1)
                                           (return-from verify-number nil)))
                    ((or (equal term 'IMPLIES) (equal term 'EQUIV))
                     (if (/= rest-length 2)
                         (return-from verify-number nil)))
                    ((or (equal term 'AND) (equal term 'OR))
                     (if (< rest-length 2)
                         (return-from verify-number nil)))
                    ((> rest-length 0) ; A single term, nothing may follow in its group
                     (return-from verify-number nil))))
            (dolist (term (rest phrase))
              (unless (verify-number term)
                (return-from verify-number nil)))
            t))
    (let ((syntax '("NOT" "AND" "OR" "IMPLIES" "EQUIV")))
      ;;; Check input in three steps.  First verify parentheses and quotes are
      ;;; properly paired. Propositions use valid Lisp syntax, so the read function
      ;;; will give an error if the proposition is not a well-formed S-exp.
      ;;; Second, verify that the phrase only contains valid syntax and atoms.
      ;;; Third, check that syntax terms have proper number of arguments.
      
      ;; First step - verify matching parentheses and quotes.  This step helps us
      ;; avoid potential errors when validating bad expressions in step two.
      (with-input-from-string (phrase input)
        (unless (ignore-errors (read phrase))
          (return-from wfp-checker nil)))
      
      ;; Second step - Verify proper terms and syntax are used.
      (do* ((i 0 (+ i 1)))
        ((equal i (length input)))
        (let* ((next-char (char input i))
               (symbol (make-array 0 :element-type 'character
                                     :fill-pointer 0
                                     :adjustable t)))
          ;; Obtain the next symbol
          (when (equal next-char (char "(" 0))
            (setf next-char (char input (setf i (+ i 1))))
            (if (equal #\" next-char) ; Checks for quoted atomiic expressions
                (progn (vector-push-extend next-char symbol)
                       (loop do (setf next-char (char input (setf i (+ i 1))))
                          (vector-push-extend next-char symbol)
                        while (not (equal next-char #\")))
                       (setf next-char (char input (setf i (+ i 1))))
                       (unless (equal next-char (char ")" 0)) ; ) must follow closing quote
                         (retun-from wfp-checker nil)))
                (loop do ; Otherwise, just build up our symbol
                  (vector-push-extend next-char symbol)
                  (setf next-char (char input (setf i (+ i 1))))
                 while (not (member next-char (list #\Space (char "(" 0) (char ")" 0)))))))
          
          ;; Verify symbol is of the proper form
          (block validate
            (unless (member next-char (list #\Space (char "(" 0) (char ")" 0)))
              (return-from wfp-checker nil)) ; Symbol not preceeded by (
            (when (equal symbol "") ; Nothing to process
              (return-from validate t))
            (when (and (equal (char symbol 0) #\") ; Quoted alphanumeric
                       (equal (char symbol (- (length symbol) 1)) #\"))
              (return-from validate t))
            (when (and (equal (length symbol) 1) ; Single characters
                       (equal next-char (char ")" 0))) ; Term properly enclosed
              (when (upper-case-p (char symbol 0)) ; Make sure uppercase!
                (return-from validate t)
              (return-from wfp-checker nil))) ; Single char, not uppercase letter
            (when (digit-char-p (char symbol 1)) ; Uppercase letter, then numbers
              (do ((j 1 (+ j 1)))
                ((equal j (length symbol)))
                (unless (digit-char-p (char symbol j)) ; Oops! Not all numbers
                  (return-from wfp-checker nil)))
              (return-from validate t)) ; Yes!  Letter, then all numbers
            (when (find symbol syntax :test #'equal) ; Look to be in syntax
              (return-from validate t))
            (return-from wfp-checker nil)) ; All tests failed
          
          ;; Uncomment below to display what was worked on
          ;;(princ symbol)
          ;;(if (member next-char (list #\Space (char ")" 0)))
          ;;    (princ next-char))
          (if (equal next-char (char "(" 0)) ; Phrase lacked spaces before (
              (setf i (- i 1))))) ; Go back to fix this
      
      ;; Third step - verify proper number of arguments for syntax words
      (with-input-from-string (phrase input)
        (unless (verify-number (read phrase))
          (return-from wfp-checker nil)))
      t)))

(defun TruthValue (assignment wfp)
  "Evalutes wfp, a list, using rules given by assignment, which is a
  list of pairs of the form (P t), where P is a term and t (or nil) is its
  value for wfp."
  (let ((wfp-string (format nil "~S" wfp)))
    (flet ((replace-all (string part replacement &key (test #'char=))
              "(From Common Lisp Cookbook)
              Returns a new string in which all the occurences of the part 
              is replaced with replacement."
              (with-output-to-string (out)
                (loop with part-length = (length part)
                      for old-pos = 0 then (+ pos part-length)
                      for pos = (search part string
                                        :start2 old-pos
                                        :test test)
                      do (write-string string out
                                       :start old-pos
                                       :end (or pos (length string)))
                      when pos do (write-string replacement out)
                 while pos))))
      (unless (wfp-checker wfp-string)
        (return-from TruthValue "Oops!  Not a well-formed propositon"))
      (defun implies (a b)
        "Need to evaluate wfp.  Defined globally because defining in the flet
        block made Lisp confused for some reason."
        (or (not a) b))
      (dolist (term assignment)
        (setf wfp-string (replace-all wfp-string
                           (concatenate 'string "(" (symbol-name (first term)) ")")
                           (concatenate 'string "(eval " (symbol-name (second term)) ")"))))
      (with-input-from-string (in wfp-string)
        (eval (read in))))))

(defun wfp-checkerFOL (input)
  "Input is a string of the proposition to check.
  Returns t if input is a well-formed proposition, nil otherwise.
  (Since input a str, double quotes of the propsotion must be escaped, i.e. use \".)
  (AND/OR can take any number of arguments and be a wfp still.)"
  (labels ((verify-arg-number (phrase &key is-arg)
            "Phrase is a list representing a proposition.  Recursively verifies each
            syntax word has proper num of arguments.  Returns nil if not.
            :flag sends message if phrase is part of predicate or function."
            (unless (listp phrase)
              (setf phrase (list phrase)))
            (let ((term (first phrase))
                  (rest-length (length (rest phrase))))
              ;; Cases determine type of term we're looking at.  IF it matches
              ;; one form completely, it will pass through this cond without
              ;; returning nil to the overall function.
              (cond ((equal term 'NOT) (if (/= rest-length 1)
                                           (return-from verify-arg-number nil)))
                    ((or (equal term 'IMPLIES) (equal term 'EQUIV))
                     (if (/= rest-length 2)
                         (return-from verify-arg-number nil)))
                    ((or (equal term 'AND) (equal term 'OR))
                     (if (< rest-length 2)
                         (return-from verify-arg-number nil)))
                    ((or (equal term 'ALL) (equal term 'EXISTS))
                     (unless (and (= rest-length 2)
                                  (listp (third phrase))
                                  (or (equal (second phrase) '|u|)
                                     (equal (second phrase) '|v|)
                                     (equal (second phrase) '|w|)
                                     (equal (second phrase) '|x|)
                                     (equal (second phrase) '|y|)
                                     (equal (second phrase) '|z|)))
                       (return-from verify-arg-number nil)))
                    ((find (char (symbol-name term) 0) "fgh")
                     (if (= rest-length 0)
                         (return-from verify-arg-number nil)))
                    (is-arg t)
                    ((find (char (symbol-name term) 0) "abcdeuvwxyz")
                     (if (not is-arg)
                         (return-from verify-arg-number nil)))))
            ;; Iterate on the remaining terms of the phrase
            (dolist (term (rest phrase))
              (if (or (find (char (symbol-name (first phrase)) 0) "fgh")
                      (and (= (length (symbol-name (first phrase))))
                           (upper-case-p (char (symbol-name (first phrase)) 0))))
                  (unless (verify-arg-number term :is-arg t)
                    (return-from verify-arg-number nil))
                  (unless (verify-arg-number term)
                    (return-from verify-arg-number nil))))
            t))
    (let ((syntax '("NOT" "AND" "OR" "IMPLIES" "EQUIV" "ALL" "EXISTS")))
      ;;; Check input in three steps.  First verify parentheses and quotes are
      ;;; properly paired. Propositions use valid Lisp syntax, so the read function
      ;;; will give an error if the proposition is not a well-formed S-exp.
      ;;; Second, verify that the phrase only contains valid syntax and atoms.
      ;;; Third, check that syntax terms have proper number of arguments.
      
      ;; First step - verify matching parentheses and quotes.  This step helps us
      ;; avoid potential errors when validating bad expressions in step two.
      (with-input-from-string (phrase input)
        (unless (ignore-errors (read phrase))
          (return-from wfp-checkerFOL nil)))
      
      ;; Second step - Verify proper terms and syntax are used.
      (do* ((i 0 (+ i 1)))
        ((equal i (length input)))
        (let* ((next-char (char input i))
               (symbol (make-array 0 :element-type 'character
                                     :fill-pointer 0
                                     :adjustable t)))
          ;; Obtain the next symbol
          (when (or (equal next-char (char "(" 0)) (equal next-char #\|))
            (if (equal next-char #\|) (setf i (- i 1))) ; Don't skip | symbol
            (setf next-char (char input (setf i (+ i 1))))
            (if (equal #\" next-char) ; Checks for quoted atomiic expressions
                (progn (vector-push-extend next-char symbol)
                       (loop do (setf next-char (char input (setf i (+ i 1))))
                          (vector-push-extend next-char symbol)
                        while (not (equal next-char #\")))
                       (setf next-char (char input (setf i (+ i 1))))
                       (unless (equal next-char (char ")" 0)) ; ) must follow closing quote
                         (retun-from wfp-checkerFOL nil)))
                (loop do ; Otherwise, just build up our symbol
                  (vector-push-extend next-char symbol)
                  (setf next-char (char input (setf i (+ i 1))))
                 while (not (member next-char (list #\Space (char "(" 0) (char ")" 0)))))))
          
          ;; Verify symbol is of the proper form
          (block validate
            (unless (member next-char (list #\Space (char "(" 0) (char ")" 0)))
              (return-from wfp-checkerFOL nil)) ; Symbol not preceeded by (
            (when (equal symbol "") ; Nothing to process
              (return-from validate t))
            (when (and (equal (char symbol 0) #\") ; Quoted alphanumeric
                       (equal (char symbol (- (length symbol) 1)) #\"))
              (return-from validate t))
            (when (equal (length symbol) 1) ; Single characters
              (when (upper-case-p (char symbol 0)) ; Is uppercase?
                (return-from validate t)))
            (when (and (equal (length symbol) 3)
                       (equal (char symbol 0) #\|)
                       (equal (char symbol 2) #\|)
                       (find (char symbol 1) "abcdefghuvwyxz"))
              (return-from validate t))
            (when (digit-char-p (char symbol 1)) ; Uppercase letter, then numbers
              (do ((j 1 (+ j 1)))
                ((equal j (length symbol)))
                (unless (digit-char-p (char symbol j)) ; Oops! Not all numbers
                  (return-from wfp-checkerFOL nil)))
              (return-from validate t)) ; Yes!  Letter, then all numbers
            (when (find symbol syntax :test #'equal) ; Look to be in syntax
              (return-from validate t))
            (return-from wfp-checkerFOL nil)) ; All tests failed
          ;; Uncomment below to display what was worked on
          ;;(princ symbol)
          ;;(if (member next-char (list #\Space (char ")" 0)))
          ;;    (princ next-char))
          (if (equal next-char (char "(" 0)) ; Phrase lacked spaces before (
              (setf i (- i 1))))) ; Go back to fix this
      
      ;; Third step - verify proper number of arguments for syntax words
      (with-input-from-string (phrase input)
        (unless (verify-arg-number (read phrase))
          (return-from wfp-checkerFOL nil)))
      t)))
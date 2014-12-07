(defun wfp-checker (input)
  "Input is a string of the proposition to check.
  Note: Quotes of the propsotion must be escaped, i.e. use \".
  Returns t if input is a well-formed proposition, nil otherwise."
  (let ((syntax '("NOT" "AND" "OR" "IMPLIES" "EQUIV"))
        (stack '()))
    ;;; Check input in two steps.  First verify parentheses and quotes are
    ;;; properly paired. Propositions use valid Lisp syntax, so the read function
    ;;; will give an error if the proposition is not a well-formed S-exp.
    ;;; Then verify that the phrase only contains valid syntax and atoms.
    (with-input-from-string (phrase input)
      (unless (ignore-errors (read phrase))
        (return-from wfp-checker nil)))
    (do* ((i 0 (+ i 1)))
      ((equal i (length input)))
      (let* ((next-char (char input i))
             (symbol (make-array 0 :element-type 'character
                                   :fill-pointer 0
                                   :adjustable t)))
        ;; Obtain the next symbol
        (when (equal next-char (char "(" 0))
          (princ next-char)
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
            (return-from validate nil))
          (when (and (equal (char symbol 0) #\") ; Quoted alphanumeric
                     (equal (char symbol (- (length symbol) 1)) #\"))
            (return-from validate nil))
          (when (and (equal (length symbol) 1) ; Single characters
                     (equal next-char (char ")" 0))) ; Term properly enclosed
            (when (upper-case-p (char symbol 0)) ; Make sure uppercase!
              (return-from validate nil)
            (return-from wfp-checker nil))) ; Single char, not uppercase letter
          (when (digit-char-p (char symbol 1)) ; Uppercase letter, then numbers
            (do ((j 1 (+ j 1)))
              ((equal j (length symbol)))
              (unless (digit-char-p (char symbol j)) ; Oops! Not all numbers
                (return-from wfp-checker nil)))
            (return-from validate nil)) ; Yes!  Letter, then all numbers
          (when (find symbol syntax :test #'equal) ; Look to be in syntax
            (return-from validate nil))
          (return-from wfp-checker nil)) ; All tests failed
        
        ;; Display what we've worked on - to use for stack later
        (princ symbol)
        (if (member next-char (list #\Space (char ")" 0)))
            (princ next-char))
        (if (equal next-char (char "(" 0)) ; Phrase lacked spaces before (
            (setf i (- i 1))))) ; Go back to fix this
    t))

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
      (print wfp-string)
      (with-input-from-string (in wfp-string)
        (eval (read in))))))
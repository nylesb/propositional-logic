(defun wfp-checker (input)
  "Input is a string of the proposition to check.
  Note: Quotes of the propsotion must be escaped, i.e. use \".
  Returns t if input is a well-formed proposition, nil otherwise."
  (let ((syntax '(NOT AND OR IMPLIES EQUIV))
        (stack '()))
    ;;; Check input in two steps.  First verify parentheses and quotes are
    ;;; properly paired. Propositions use valid Lisp syntax, so the read function
    ;;; will give an error if the proposition is not a well-formed S-exp.
    ;;; Then verify that the phrase only contains valid syntax and atoms.
    (princ (format nil "Working on ~A~%  " input))
    (with-input-from-string (phrase input)
      (unless (ignore-errors (read phrase))
        (return-from wfp-checker nil)))
    (do* ((i 0 (+ i 1)))
      ((equal i (length input)))
      (let* ((next-char (char input i))
             (symbol (make-array 1 :element-type 'character
                                   :fill-pointer 1
                                   :adjustable t
                                   :initial-element next-char)))
        ;; Verify we have single uppercase, one uppercase followed by numbers,
        ;; or a valid syntax word
        (when (upper-case-p next-char)
          (setf next-char (char input (setf i (+ i 1))))
          (vector-push-extend next-char symbol)
          (if (equal next-char #\Space) ; Was single uppercase
            (return))
          (when (upper-case-p next-char) ; Must be only uppercase now
            (loop do (setf next-char (char input (setf i (+ i 1))))
              (if (equal next-char #\Space) (return))
              (if (not (upper-case-p next-char)) ; Invalid form
                  (return-from wfp-checker nil))
              (vector-push-extend next-char symbol)
             while (not (equal next-char #\Space))))
          (when (digit-char-p next-char) ; Must be only numbers now
            (loop do (setf next-char (char input (setf i (+ i 1))))
              (when (equal next-char (char ")" 0))
                (vector-push-extend next-char symbol)
                (return))
              (if (not (digit-char-p next-char)) ; Invalid form
                  (return-from wfp-checker nil))
              (vector-push-extend next-char symbol)
             while (not (equal next-char (char ")" 0))))))
        ;; Discard quotes
        (when (equal #\" next-char)
          (loop do (setf next-char (char input (setf i (+ i 1))))
            while (not (equal next-char #\"))))
        (princ symbol)))
    t))
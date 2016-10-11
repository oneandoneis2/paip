(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name")

(defparameter *postfix-titles*
  '(MD BSc MSc pHD Jr Sr)
  "Stuff that can come after a person's name")

(defun first-name (name)
  "Select the first name from a name represented as a list"
  (if (member (first name) *titles*)
    (first-name (rest name))
    (first name)))

(defun last-name (name)
  "Return the last name from a name represented as a list"
  (first (last (remove-if (lambda (test) (member test *postfix-titles*)) name))))

(setf names '((John Q Public) (Malcolm X) (Admiral Grace Murray Hopper)
              (Spot) (Aristotle) (A A Milne) (Z Z Top) (Sir Larry Olivier)
              (Miss Scarlet)))

(mapcar #'first-name names)

;(trace first-name)
;(first-name '(Madam major general paula jones))
;1. Trace: (FIRST-NAME '(MADAM MAJOR GENERAL PAULA JONES))
;2. Trace: (FIRST-NAME '(MAJOR GENERAL PAULA JONES))
;3. Trace: (FIRST-NAME '(GENERAL PAULA JONES))
;4. Trace: (FIRST-NAME '(PAULA JONES))
;4. Trace: FIRST-NAME ==> PAULA
;3. Trace: FIRST-NAME ==> PAULA
;2. Trace: FIRST-NAME ==> PAULA
;1. Trace: FIRST-NAME ==> PAULA
;PAULA
;(untrace first-name)

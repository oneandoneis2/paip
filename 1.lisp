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

; 1.1
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

; 1.2
(defun power (num ex)
  "Return num raised to the ex power"
  (cond ((eq ex 0) 1)
        ((eq ex 1) num)
        (t (* num (power num (- ex 1))))))

; 1.3
(defun count_atom (ex)
  "Count all non-nil atoms"
  (cond ((null ex) 0)
        ((atom ex) 1)
        (t (+ (count_atom (car ex))
              (count_atom (cdr ex))))))

(defun first-name (name)
  "Select the first name from a name represented as a list"
  (first name))

(setf names '((John Q Public) (Malcolm X) (Admiral Grace Murray Hopper)
              (Spot) (Aristotle) (A A Milne) (Z Z Top) (Sir Larry Olivier)
              (Miss Scarlet)))

(mapcar #'first-name names)

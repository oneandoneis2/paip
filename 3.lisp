;3.1
(let* ((x 6) (y (* x x))) (princ (+ x y))) ; => 42
((lambda (x) ((lambda (y) (princ (+ x y))) (* x x))) 6) ; => 42

; 3.3
(defun print-dotted (lst)
  (cond ((atom lst) (princ lst))
        (t (princ "(")
           (print-dotted (car lst))
           (print-rest (cdr lst))
           (princ ")")
           lst)))
; 3.4
(defun print-rest (lst)
  (cond ((null lst))
        ((atom lst) (princ " . ")
                    (princ lst))
        (t (princ " ")
           (print-dotted (car lst))
           (print-rest (cdr lst)))))

;3.9
(defun length-by-reduce (lst)
  (reduce #'+ (mapcar (lambda (x) 1) lst)))

;3.10
; lcm - least common multiple
; nreconc takes two lists, destructively reverses the first & adds the second as a tail

;3.11
; acons

;3.12
(format t "~@(~{~a~^ ~}.~)" '(this is a list of words))

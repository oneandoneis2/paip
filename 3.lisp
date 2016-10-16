;3.1
(let* ((x 6) (y (* x x))) (princ (+ x y))) ; => 42
((lambda (x) ((lambda (y) (princ (+ x y))) (* x x))) 6) => 42

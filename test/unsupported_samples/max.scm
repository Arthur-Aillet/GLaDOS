(define maximum
  (lambda (x y)
    (if (> x y)
        x
        y)))

(maximum 3 9)
(maximum 9 3)
(maximum 3 0)
(maximum 3 -9)
(maximum -3 -9)
(maximum 0 0)
(maximum -1 -2)
(maximum -1 2)

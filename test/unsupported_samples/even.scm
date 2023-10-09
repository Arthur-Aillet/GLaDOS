(define even
  (lambda (n)
    (if (eq? (mod n 2) 0)
        #t
        #f)))

(even 7)
(even 8)
(even 6)
(even 2)
(even 0)

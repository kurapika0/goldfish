(import (chezscheme))

(map
  (lambda (y)
    (filter (lambda (z) (zero? (modulo z 3)))
            (map (lambda (x) (* x x)) (iota 10000))))
  (iota 10000))

(define start-time (current-time))
(define result
  (map
    (lambda (y)
      (filter (lambda (z) (zero? (modulo z 3)))
              (map (lambda (x) (* x x)) (iota 10000))))
    (iota 1000)))
(define end-time (current-time))

(display "Length of each result: ")
(display (length (car result)))
(newline)

(display "Elapsed time: ")
(display (time-difference end-time start-time))

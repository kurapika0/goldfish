(import (srfi srfi-1)
        (scheme time))

(map
  (lambda (y)
    (filter (lambda (z) (zero? (modulo z 3)))
            (map (lambda (x) (* x x)) (iota 10000))))
  (iota 1000))

(define start-time (current-jiffy))
(define result
  (map
    (lambda (y)
      (filter (lambda (z) (zero? (modulo z 3)))
              (map (lambda (x) (* x x)) (iota 10000))))
    (iota 1000)))
(define end-time (current-jiffy))

(display "Length of each result: ")
(display (length (car result)))
(newline)

(display "Elapsed time: ")
(display (/ (- end-time start-time) 1000000000.0))

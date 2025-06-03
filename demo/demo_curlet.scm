(import (liii list))
(define (print-curlet-length)
  (display "Length of curlet ")
  (display (length (rootlet)))
  (newline)
)

(print-curlet-length)
(import (liii oop))
(print-curlet-length)
(import (liii oop))
(print-curlet-length)

(let->list (rootlet))

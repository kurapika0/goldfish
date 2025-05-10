(import (scheme time))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))


(timing "prim: " (lambda () (repeat 10000 (lambda () (number->string 65536)))))

(define-case-class rich-integer2 ((data integer?))
  (define (%to-string)
    (number->string data)))

(timing "rich-integer: " (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :to-string)))))
(timing "rich-integer2: " (lambda () (repeat 10000 (lambda () ((rich-integer2 65536) :to-string)))))

(define (rint x)
  (varlet (funclet rint) 'data x)

  (lambda (msg . args)
    (case msg
      ((:to-string)
       (((funclet rint) '%to-string)))
      ((:to-rich-string)
       (((funclet rint) '%to-rich-string))))))

(varlet (funclet rint) '%to-string
  (with-let (funclet rint)
    (define (%to-string)
      (number->string data))
    %to-string))

(varlet (funclet rint) '%to-rich-string
  (with-let (funclet rint)
    (define (%to-rich-string)
      (rich-string (%to-string)))
    %to-rich-string))

(varlet (funclet rint) '%sqrt
  (with-let (funclet rint)
    (define (%sqrt)
      (if (< data 0)
          (value-error
            (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
          (inexact->exact (floor (sqrt data)))))))



(timing "rint: " (lambda () (repeat 10000 (lambda () ((rint 65536) :to-string)))))
(timing "rint%sqrt: " (lambda () (repeat 10000 (lambda () ((rint 65536) :sqrt)))))
(timing "rich-integer%sqrt: " (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :sqrt)))))

; slow because of rich-string
(timing "rint%to-rich-string " (lambda () (repeat 1000 (lambda () (((rint 65536) :to-rich-string) :length)))))

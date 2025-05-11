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


(timing "prim%to-string: " (lambda () (repeat 10000 (lambda () (number->string 65536)))))

(define-case-class rich-integer2 ((data integer?))
  (define (%to-string)
    (number->string data)))

(timing "rich-integer%to-string: " (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :to-string)))))
(timing "rich-integer2%to-string: " (lambda () (repeat 10000 (lambda () ((rich-integer2 65536) :to-string)))))

(define (rint x)
  (lambda (msg . args)
    (let ((env (funclet rint)))
      (case msg
        ((:to-string)
         (begin
           (varlet env 'data x)
           ((env '%to-string)) 
           (cutlet env 'data)))
        ((:to-rich-string)
         (begin
           (varlet env 'data x)
           ((env '%to-rich-string))
           (cutlet env 'data)))))))

(with-let (funclet rint)
  (define (%to-string)
    (number->string data))
  (varlet (funclet rint) '%to-string %to-string))

(with-let (funclet rint)
  (define (%to-rich-string)
    (rich-string (%to-string)))
  (varlet (funclet rint) '%to-rich-string %to-rich-string))

(with-let (funclet rint)
  (define (%sqrt)
        (if (< data 0)
            (value-error
              (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
            (inexact->exact (floor (sqrt data)))))
  (varlet (funclet rint) '%sqrt %sqrt))



(timing "rint%to-string: " (lambda () (repeat 10000 (lambda () ((rint 65536) :to-string)))))

(define (prim-sqrt data)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

(timing "prim%sqrt: " (lambda () (repeat 10000 (lambda () (prim-sqrt 65536)))))
(timing "rint%sqrt: " (lambda () (repeat 10000 (lambda () ((rint 65536) :sqrt)))))
(timing "rich-integer%sqrt: " (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :sqrt)))))

; slow because of rich-string
(timing "rint%to-rich-string " (lambda () (repeat 1000 (lambda () (((rint 65536) :to-rich-string) :length)))))

;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii range)
(import (liii lang))
(export range)
(begin

(define-case-class range
  ((start integer?) (end integer?) (step integer?) (inclusive? boolean?))

(define* (@inclusive start end (step 1))
  (range start end step #t))

(define (%empty?)
  (or (and (> start end) (> step 0))
      (and (< start end) (< step 0))
      (and (= start end) (not inclusive?))))

(define (%map)
  (define current start)
  (let loop ((current start)) 
       (cond 
         ((%empty?) (display "empty range\n")) 
         ((or (and (> step 0) (> current end)) 
              (and (< step 0) (< current end))) 
          (newline)) 
         (else 
           (display (map-func current)) 
           (display " ") 
           (loop (+ current step))))))

) ; define-case-class
) ; begin
) ; define-library


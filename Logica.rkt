#lang racket
;; (actualizar '("a" "b" "-"))
(define(actualizar list1)
  (list (actualizar_aux list1) '(1 1 1) '(0 0 0 0) '('() '() '() '()) '("cartas") (contar list1)))

(define(actualizar_aux list1)
  (cond((null? list1) '("Crupier"))
       ((equal?  (car list1) "-") (actualizar_aux (cdr list1)))
       (else (cons (car list1) (actualizar_aux (cdr list1))))
       ))

(define(contar list1)
  (cond((null? list1) 0)
       ((equal? (car list1) "-") (+ 0 (contar (cdr list1))))
       (else (+ 1 (contar (cdr list1))))
       ))

(provide (all-defined-out))
#lang racket
;; (actualizar '("a" "b" "-"))
(define(actualizar list1)
  (cond((equal? (contar list1) 1) (list (actualizar_aux list1) '(1) '(0 0) '('() '()) '("cartas") 1))
       ((equal? (contar list1) 2) (list (actualizar_aux list1) '(1 1) '(0 0 0) '('() '() '()) '("cartas") 2))
       (else (list (actualizar_aux list1) '(1 1 1) '(0 0 0 0) '('() '() '() '()) '("cartas") 3))
       ))

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
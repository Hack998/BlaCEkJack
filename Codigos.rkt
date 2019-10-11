;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Codigos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Factorial
;; (factorial 5)
(define(factorial value)
  (cond((equal? value 0) 1)
       ((> value 0)(* value (factorial (- value 1))))
       ))

;; Fibonacci
;; (fibonacci 6)
(define(fibonacci value)
  (cond((equal? value 0) 0)
       ((equal? value 1) 1)
       ((> value 1)(+ (fibonacci(- value 1)) (fibonacci(- value 2))))
       ))

;; Miembro
;; (miembro? 'a '(a b c))
;; (miembro? 'a '(b c d))
(define(miembro? value list1)
  (cond((null? list1) #false)
       ((equal? value (car list1)) #true)
       (else (miembro? value (cdr list1)))
       ))

;; Eliminate
;; (eliminate 'a '(a b c))
;; (eliminate 'a '(b c d))
(define(eliminate value list1)
  (cond((null? list1) list1)
       ((equal? value (car list1)) (eliminate value (cdr list1)))
       (else (cons (car list1) (eliminate value (cdr list1))))
       ))

;; QuickSort
;; (quickSort '(3 2 1))
;; (quickSort '(2 3 4 1 1 2 5))
(define(quickSort list1)
   (cond((null? list1) list1)
        (else (qs_aux (car list1) (cdr list1)))
        ))

(define(qs_aux value list1)
   (cond((null? list1) (list value))
        (else (append (quickSort(menores value list1)) (list value) (quickSort(mayores value list1))))
        ))

(define(menores value list1)
  (cond((null? list1) list1)
       ((> value (car list1)) (cons (car list1) (menores value (cdr list1))))
       (else (menores value (cdr list1)))
       ))

(define(mayores value list1)
  (cond((null? list1) list1)
       ((<= value (car list1)) (cons (car list1) (mayores value (cdr list1))))
       (else (mayores value (cdr list1)))
       ))

;; Combinar dos listas
;; (automovil '(Hatchback Suzuki Forza1 Rojo si Manual) '(Tipo Marca Modelo Color AC Transmision))
(define(automovil list1 list2)
  (cond((null? list1) '())
       ((null? list2) '())
       (else (append (list(list (car list2) (car list1))) (automovil (cdr list1) (cdr list2))))
       ))

;; Eliminar en arbol binario
;; (eliminarAB 14 '(10 (5 3 8) (15 14 18)))
;; (eliminarAB 15 '(10 (5 3 8) (15 () 18)))
;; (eliminarAB 10 '(10 (5 3 8) 18))
(define(eliminarAB value list1)
  (cond((null? list1) list1)
       ((equal? value list1) '())
       ((< value (car list1)) (list (car list1) (eliminarAB value (hijoLH list1)) (hijoRH list1)))
       ((> value (car list1)) (list (car list1) (hijoLH list1) (eliminarAB value (hijoRH list1))))
       ((equal? value (car list1)) (cond((null? (hijoLH list1)) (hijoRH list1))
                                        ((null? (hijoRH list1)) (hijoLH list1))
                                        (else (list (mayor_menor (hijoLH list1)) (eliminarAB (mayor_menor (hijoLH list1)) (hijoLH list1)) (hijoRH list1)))
                                        ))
       ))

(define(hijoLH list1)
  (cond((list? list1) (cadr list1))
       (else '())
       ))

(define(hijoRH list1)
  (cond((list? list1) (caddr list1))
       (else '())
       ))

(define(mayor_menor list1)
  (cond((null? (hijoRH list1)) list1)
        (else (mayor_menor (hijoRH list1)))
        ))

;; Rutas anchura-primero
;; (anchura '((d(b c)) (b(h)) (c(r)) (r(h)) (h(a t d)) (a()) (t())) 'd)
(define(anchura list1 value)
  (cond((null? list1) list1)
       ((equal? value (caar list1)) (append (list (caar list1)) (anchura_aux (marcar list1 value) (vecino list1 value))))
       (else (anchura (append (cdr list1) (list (car list1))) value))
       ))

(define(anchura_aux list1 list2)
  (cond((null? list2) list2)
       (else (append (list (car list2)) (anchura_aux (marcar list1 (car list2)) (append (cdr list2) (vecino list1 (car list2))))))
       ))

(define(vecino list1 value)
  (cond((equal? value (caar list1)) (cadar list1))
       (else (vecino (cdr list1) value))
       ))

(define(marcar list1 value)
  (cond((null? list1) list1)
       (else (append(list(list (caar list1) (eliminate value (cadar list1)))) (marcar (cdr list1) value)))
       ))
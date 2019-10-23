#lang racket
(require racket/draw
         net/url)

(define cards '("A1" "B1" "C1" "D1" "E1" "F1" "G1" "H1" "I1" "J1" "K1" "L1" "M1"
                "A2" "B2" "C2" "D2" "E2" "F2" "G1" "H2" "I2" "J2" "K2" "L2" "M2"
                "A3" "B3" "C3" "D3" "E3" "F3" "G3" "H3" "I3" "J3" "K3" "L3" "M3"
                "A4" "B4" "C4" "D4" "E4" "F4" "G4" "H4" "I4" "J4" "K4" "L4" "M4"))

;; ==================================================================================================================================
;; Actualize
;; Initializes the data of the players
;; list1: List with the names of the players
;; listnum: List containing the victories of the players
;; ==================================================================================================================================
(define(actualize list1 listnum)
  (cond((equal? (count list1) 1) (list (actualize_aux list1) (list (car listnum) (cadr listnum)) '(0 0) '(() ()) (shuffleDeck cards 51) 1))
       ((equal? (count list1) 2) (list (actualize_aux list1) (list (car listnum) (cadr listnum) (caddr listnum)) '(0 0 0) '(() () ()) (shuffleDeck cards 51) 2))
       (else (list (actualize_aux list1) (list (car listnum) (cadr listnum) (caddr listnum) (cadddr listnum)) '(0 0 0 0) '(() () () ()) (shuffleDeck cards 51) 3))
       ))

(define(actualize_aux list1)
  (cond((null? list1) '("Crupier"))
       ((equal?  (car list1) "-") (actualize_aux (cdr list1)))
       (else (cons (car list1) (actualize_aux (cdr list1))))
       ))

;; ==================================================================================================================================
;; Start values
;; Give each player his score
;; list1: List with all the data players
;; ==================================================================================================================================
(define (start_values list1)
  (cond((equal? (cadr (cddddr list1)) 1) (list (car list1) (cadr list1) (list (start_value_aux (car(cadddr list1)) (caaddr list1)) (start_value_aux (cdadr(cadddr list1)) (car(cdaddr list1)))) (cadddr list1) (cdr(cdddar(cddddr list1))) (cadr(cddddr list1))))
       ((equal? (cadr (cddddr list1)) 2) (list (car list1) (cadr list1) (list (start_value_aux (car(cadddr list1)) (caaddr list1)) (start_value_aux (cadr(cadddr list1)) (car(cdaddr list1))) (start_value_aux (cdaddr(cadddr list1)) (cadr(cdaddr list1)))) (cadddr list1) (cdddr(cdddar(cddddr list1))) (cadr(cddddr list1))))
       (else (list (car list1) (cadr list1) (list (start_value_aux (car(cadddr list1)) (caaddr list1)) (start_value_aux (cadr(cadddr list1)) (car(cdaddr list1))) (start_value_aux (caddr(cadddr list1)) (cadr(cdaddr list1))) (start_value_aux (cdr(cadddr(cadddr list1))) (caddr(cdaddr list1)))) (cadddr list1) (cdr(cddddr(cdddar(cddddr list1)))) (cadr(cddddr list1))))
       ))

(define (start_value_aux list1 score)
  (cond((null? list1) score)
       (else (cond ((equal? (string-ref (car list1) 0) #\A)
                    (cond((> score 10) (start_value_aux (cdr list1) (+ 1 score)))
                         (else (start_value_aux (cdr list1) (+ 11 score)))))
                   ((equal? (string-ref (car list1) 0) #\B)
                    (start_value_aux (cdr list1) (+ 2 score)))
                   ((equal? (string-ref (car list1) 0) #\C)
                    (start_value_aux (cdr list1) (+ 3 score)))
                   ((equal? (string-ref (car list1) 0) #\D)
                    (start_value_aux (cdr list1) (+ 4 score)))
                   ((equal? (string-ref (car list1) 0) #\E)
                    (start_value_aux (cdr list1) (+ 5 score)))
                   ((equal? (string-ref (car list1) 0) #\F)
                    (start_value_aux (cdr list1) (+ 6 score)))
                   ((equal? (string-ref (car list1) 0) #\G)
                    (start_value_aux (cdr list1) (+ 7 score)))
                   ((equal? (string-ref (car list1) 0) #\H)
                    (start_value_aux (cdr list1) (+ 8 score)))
                   ((equal? (string-ref (car list1) 0) #\I)
                    (start_value_aux (cdr list1) (+ 9 score)))
                   (else
                    (start_value_aux (cdr list1) (+ 10 score)))
                   ))
       ))

;; ==================================================================================================================================
;; Crupier values
;; Restart the Crupier score
;; list1: List with all the data players
;; ==================================================================================================================================
(define (crupier_values list1)
  (cond((equal? (cadr (cddddr list1)) 1) (list (car list1) (cadr list1)
                                               (list (caaddr list1) (start_value_aux (list(caadr(cadddr list1))) (car(cdaddr list1))))
                                               (cadddr list1) (car(cddddr list1)) (cadr(cddddr list1))))
       ((equal? (cadr (cddddr list1)) 2) (list (car list1) (cadr list1)
                                               (list (caaddr list1) (car(cdaddr list1)) (start_value_aux (list(caaddr(cadddr list1))) (cadr(cdaddr list1))))
                                               (cadddr list1) (car(cddddr list1)) (cadr(cddddr list1))))
       (else (list (car list1) (cadr list1)
                   (list (caaddr list1) (car(cdaddr list1)) (cadr(cdaddr list1)) (start_value_aux (list(car(cadddr(cadddr list1)))) (caddr(cdaddr list1))))
                   (cadddr list1) (car(cddddr list1)) (cadr(cddddr list1))))
       ))

;; ==================================================================================================================================
;; Count
;; Get the length of a list
;; list1: List you want to know the length
;; ==================================================================================================================================
(define(count list1)
  (cond((null? list1) 0)
       ((equal? (car list1) "-") (+ 0 (count (cdr list1))))
       ((equal? (car list1) "Crupier") (+ 0 (count (cdr list1))))
       (else (+ 1 (count (cdr list1))))
       ))

;; ==================================================================================================================================
;; Bigger
;; Search the maximum of a list
;; list1: List to which you want to find the maximum
;; ==================================================================================================================================
(define(bigger list1)
  (cond((null? list1) '())
       (else (bigger_aux (car list1) (cdr list1)))
       ))

(define(bigger_aux ele list1)
  (cond((null? list1) ele)
       ((< ele (car list1)) (bigger_aux (car list1) (cdr list1)))
       (else (bigger_aux ele (cdr list1)))
       ))

;; ==================================================================================================================================
;; Eliminate
;; Remove an item from a list
;; list1: List that an object will be deleted
;; num: Indicate which is the item to remove
;; ==================================================================================================================================
(define(eliminate list1 num)
  (list (eliminate_aux(car list1) num) (eliminate_aux(cadr list1)num) (eliminate_aux(caddr list1)num) (cadddr list1) (car(cddddr list1)) (cadr(cddddr list1))))

(define (eliminate_aux list1 num)
  (cond((equal? num 0) (cdr list1))
       (else (cons (car list1) (eliminate_aux (cdr list1) (- num 1))))
       ))

;; ==================================================================================================================================
;; Winner
;; Select the winner
;; list1: List containing the possible winners
;; num: Indicate how many winners there can be
;; ==================================================================================================================================
(define(winner list1 num)
  (cond((equal? num 0) list1)
       ((< 21 (cadar list1)) (cons (car list1) (winner (cdr list1) num)))
       (else (cons (list (caar list1) (cadar list1) "Yes" (+ 1 (car(cdddar list1)))) (winner (cdr list1) 0)))
       ))

;; ==================================================================================================================================
;; Table
;; Organize the possible winners
;; list1: List passed by user interface
;; ==================================================================================================================================
(define(table list1)
  (append (list (cadr(cddddr list1))) (winner(table_aux list1 (bigger (caddr list1)))1) (list(car list1))))

(define(table_aux list1 max)
  (cond((null? max) '())
       ((equal? max (caaddr list1)) (cons (list (caar list1) (caaddr list1) "" (caadr list1))
                                          (table_aux (eliminate list1 0) (bigger (eliminate_aux(caddr list1) 0)))))
       ((equal? max (car (cdaddr list1))) (cons(list (cadar list1) (car (cdaddr list1)) "" (cadadr list1))
                                          (table_aux (eliminate list1 1) (bigger (eliminate_aux(caddr list1) 1)))))
       ((equal? max (cadr(cdaddr list1))) (cons(list (caddar list1) (cadr(cdaddr list1)) "" (car(cddadr list1)))
                                          (table_aux (eliminate list1 2) (bigger (eliminate_aux(caddr list1) 2)))))
       ((equal? max (caddr(cdaddr list1))) (cons(list (car(cdddar list1)) (caddr(cdaddr list1)) "" (cadr(cddadr list1)))
                                           (table_aux (eliminate list1 3) (bigger (eliminate_aux(caddr list1) 3)))))
       ))

;; ==================================================================================================================================
;; Order
;; Organize the streaks according to the name of the player
;; list1: List passed by user interface
;; num: Number of players
;; ==================================================================================================================================
(define(order num list1)
  (cond((equal? num 1) (cond((null? (caddr list1))'())
                            ((equal? (caaddr list1) (caar list1)) (cons (car(cdddar list1)) (order num (list (car list1) (cadr list1) (cdaddr list1)))))
                            (else (cons (cadr(cddadr list1)) (order num (list (car list1) (cadr list1) (cdaddr list1)))))
                            ))
       ((equal? num 2)(cond((null? (car(cdddr list1)))'())
                           ((equal? (caar(cdddr list1)) (caar list1)) (cons (car(cdddar list1)) (order num (list (car list1) (cadr list1) (caddr list1) (cdar(cdddr list1))))))
                           ((equal? (caar(cdddr list1)) (caadr list1)) (cons (cadr(cddadr list1)) (order num (list (car list1) (cadr list1) (caddr list1)  (cdar(cdddr list1))))))
                           (else (cons (caddr(cdaddr list1)) (order num (list (car list1) (cadr list1) (caddr list1)  (cdar(cdddr list1))))))
                           ))
       (else (cond((null? (car(cddddr list1)))'())
                  ((equal? (caar(cddddr list1)) (caar list1)) (cons (car(cdddar list1)) (order num (list (car list1) (cadr list1) (caddr list1) (cadddr list1) (cdar(cddddr list1))))))
                  ((equal? (caar(cddddr list1)) (caadr list1)) (cons (cadr(cddadr list1)) (order num (list (car list1) (cadr list1) (caddr list1) (cadddr list1) (cdar(cddddr list1))))))
                  ((equal? (caar(cddddr list1)) (caaddr list1)) (cons (caddr(cdaddr list1)) (order num (list (car list1) (cadr list1) (caddr list1) (cadddr list1) (cdar(cddddr list1))))))
                  (else (cons (cadddr(cadddr list1)) (order num (list (car list1) (cadr list1) (caddr list1) (cadddr list1) (cdar(cddddr list1))))))
                  ))
       ))

;; ==================================================================================================================================
;; Set logo
;; Select the image that corresponds to each possible card
;; cart: Class name of the letter to look for
;; num1: Identify the size of the image
;; ==================================================================================================================================
(define(set-logo cart num1)
  (cond((equal? num1 1) (cond((equal? cart "A1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CAs.png"))))
                             ((equal? cart "B1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C2.png"))))
                             ((equal? cart "C1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C3.png"))))
                             ((equal? cart "D1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C4.png"))))
                             ((equal? cart "E1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C5.png"))))
                             ((equal? cart "F1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C6.png"))))
                             ((equal? cart "G1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C7.png"))))
                             ((equal? cart "H1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C8.png"))))
                             ((equal? cart "I1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C9.png"))))
                             ((equal? cart "J1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C10.png"))))
                             ((equal? cart "K1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CJ.png"))))
                             ((equal? cart "L1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CQ.png"))))
                             ((equal? cart "M1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CK.png"))))
                             ((equal? cart "A2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TAs.png"))))
                             ((equal? cart "B2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T2.png"))))
                             ((equal? cart "C2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T3.png"))))
                             ((equal? cart "D2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T4.png"))))
                             ((equal? cart "E2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T5.png"))))
                             ((equal? cart "F2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T6.png"))))
                             ((equal? cart "G2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T7.png"))))
                             ((equal? cart "H2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T8.png"))))
                             ((equal? cart "I2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T9.png"))))
                             ((equal? cart "J2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T10.png"))))
                             ((equal? cart "K2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TJ.png"))))
                             ((equal? cart "L2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TQ.png"))))
                             ((equal? cart "M2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TK.png"))))
                             ((equal? cart "A3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RAs.png"))))
                             ((equal? cart "B3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R2.png"))))
                             ((equal? cart "C3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R3.png"))))
                             ((equal? cart "D3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R4.png"))))
                             ((equal? cart "E3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R5.png"))))
                             ((equal? cart "F3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R6.png"))))
                             ((equal? cart "G3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R7.png"))))
                             ((equal? cart "H3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R8.png"))))
                             ((equal? cart "I3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R9.png"))))
                             ((equal? cart "J3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R10.png"))))
                             ((equal? cart "K3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RJ.png"))))
                             ((equal? cart "L3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RQ.png"))))
                             ((equal? cart "M3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RK.png"))))
                             ((equal? cart "A4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PAs.png"))))
                             ((equal? cart "B4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P2.png"))))
                             ((equal? cart "C4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P3.png"))))
                             ((equal? cart "D4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P4.png"))))
                             ((equal? cart "E4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P5.png"))))
                             ((equal? cart "F4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P6.png"))))
                             ((equal? cart "G4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P7.png"))))
                             ((equal? cart "H4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P8.png"))))
                             ((equal? cart "I4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P9.png"))))
                             ((equal? cart "J4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P10.png"))))
                             ((equal? cart "K4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PJ.png"))))
                             ((equal? cart "L4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PQ.png"))))
                             ((equal? cart "M4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PK.png"))))
                             ))
       (else (cond((equal? cart "A1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CAs (1).png"))))
                  ((equal? cart "B1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C2 (1).png"))))
                  ((equal? cart "C1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C3 (1).png"))))
                  ((equal? cart "D1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C4 (1).png"))))
                  ((equal? cart "E1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C5 (1).png"))))
                  ((equal? cart "F1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C6 (1).png"))))
                  ((equal? cart "G1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C7 (1).png"))))
                  ((equal? cart "H1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C8 (1).png"))))
                  ((equal? cart "I1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C9 (1).png"))))
                  ((equal? cart "J1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/C10 (1).png"))))
                  ((equal? cart "K1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CJ (1).png"))))
                  ((equal? cart "L1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CQ (1).png"))))
                  ((equal? cart "M1") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/CK (1).png"))))
                  ((equal? cart "A2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TAs (1).png"))))
                  ((equal? cart "B2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T2 (1).png"))))
                  ((equal? cart "C2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T3 (1).png"))))
                  ((equal? cart "D2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T4 (1).png"))))
                  ((equal? cart "E2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T5 (1).png"))))
                  ((equal? cart "F2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T6 (1).png"))))
                  ((equal? cart "G2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T7 (1).png"))))
                  ((equal? cart "H2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T8 (1).png"))))
                  ((equal? cart "I2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T9 (1).png"))))
                  ((equal? cart "J2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/T10 (1).png"))))
                  ((equal? cart "K2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TJ (1).png"))))
                  ((equal? cart "L2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TQ (1).png"))))
                  ((equal? cart "M2") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TK(1).png"))))
                  ((equal? cart "A3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RAs (1).png"))))
                  ((equal? cart "B3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R2 (1).png"))))
                  ((equal? cart "C3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R3 (1).png"))))
                  ((equal? cart "D3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R4 (1).png"))))
                  ((equal? cart "E3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R5 (1).png"))))
                  ((equal? cart "F3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R6 (1).png"))))
                  ((equal? cart "G3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R7 (1).png"))))
                  ((equal? cart "H3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R8 (1).png"))))
                  ((equal? cart "I3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R9 (1).png"))))
                  ((equal? cart "J3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/R10 (1).png"))))
                  ((equal? cart "K3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RJ (1).png"))))
                  ((equal? cart "L3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RQ (1).png"))))
                  ((equal? cart "M3") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/RK (1).png"))))
                  ((equal? cart "A4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PAs (1).png"))))
                  ((equal? cart "B4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P2 (1).png"))))
                  ((equal? cart "C4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P3 (1).png"))))
                  ((equal? cart "D4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P4 (1).png"))))
                  ((equal? cart "E4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P5 (1).png"))))
                  ((equal? cart "F4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P6 (1).png"))))
                  ((equal? cart "G4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P7 (1).png"))))
                  ((equal? cart "H4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P8 (1).png"))))
                  ((equal? cart "I4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P9 (1).png"))))
                  ((equal? cart "J4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/P10 (1).png"))))
                  ((equal? cart "K4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PJ (1).png"))))
                  ((equal? cart "L4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PQ (1).png"))))
                  ((equal? cart "M4") (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/PK (1).png"))))
                  ))
       ))

;; ==================================================================================================================================
;; Random Value
;; Choose a random value smaller than set limit
;; rnd: Random real number between 0 and 1
;; limit: Limit number to choose the final random value
;; ==================================================================================================================================
(define (rndVal rnd limit)
  (cond ((> (exact-truncate (* rnd 100)) limit)
         (rndVal (random) limit))
        (else
         (exact-truncate (* rnd 100)))
   )
)

;; ==================================================================================================================================
;; Random Card Selector
;; Choose a random card value from list cards
;; crds: List of cards given
;; ==================================================================================================================================
(define (rndCard crds limit)
  (cond ((null? crds)
         '( ))
        (else
         (list-ref crds (rndVal (random) limit)))
   )
)

;; ==================================================================================================================================
;; Shuffle Cards Deck
;; Shuffle the cards list and return a randomly sorted list
;; crds: cards list
;; limit: number of cards in the list
;; ==================================================================================================================================
(define (shuffleDeck crds limit)
  (shuffleDeck_aux crds (rndVal (random) limit) limit))

(define (shuffleDeck_aux crds rnd limit)
  (cond ((equal? limit 0)
         (list (car crds)))
        (else
         (cons (list-ref crds rnd) (shuffleDeck_aux (dltCard crds rnd) (rndVal (random) (- limit 1)) (- limit 1))))
   )
)

(define (dltCard crds rnd)
  (cond ((equal? rnd 0)
         (cdr crds))
        (else
         (cons (car crds) (dltCard (cdr crds) (- rnd 1))))
   )
)

;; ==================================================================================================================================
;; Deal Cards
;; Deal cards to each player on game
;; list1: List passed by user interface
;; ==================================================================================================================================
(define (dealCards list1)
  (cond ((null? list1)
         list1)
        (else
         (list (car list1) (cadr list1) (caddr list1)
               (dealCards_aux (car (cddddr list1)) (cadr (cddddr list1)) (* (cadr (cddddr list1)) 2))
               (car (cddddr list1)) (cadr (cddddr list1)))
         )
   )
)

(define (dealCards_aux crds nPlyrs nCrds)
  (cond((equal? nPlyrs 0)
        (cons (list (car crds) (cadr crds)) '()))
       (else
        (cons (list (car crds) (cadr crds)) (dealCards_aux (cddr crds) (- nPlyrs 1) (- nCrds 2))))
   )
)

;;===================================================================================================================================
;; Deal A Card
;; Deal a card to player who ask for one
;; list1: list passed by user interface
;; plyr: number of Player who ask for card
;;===================================================================================================================================

(define (dealACard list1 plyr)
  (cond ((null? list1)
         '())
        (else
         (dealACard_aux list1 plyr (caar(cddddr list1))))
  )
)

(define (dealACard_aux list1 plyr card)
  (cond((equal? plyr 1)
        (list (car list1) (cadr list1) (append (list (+ (cardId list1 plyr card) (caaddr list1))) (cdaddr list1)) (append (list(append (list card) (car(cadddr list1)))) (cdr(cadddr list1))) (cdar(cddddr list1)) (cadr(cddddr list1))))
       ((equal? plyr 2)
        (list (car list1) (cadr list1) (append (list (caaddr list1) (+ (cardId list1 plyr card) (car(cdaddr list1)))) (cdr(cdaddr list1))) (append (list (car(cadddr list1))) (list(append (list card) (cadr(cadddr list1)))) (cddr(cadddr list1))) (cdar(cddddr list1)) (cadr(cddddr list1))))
       ((equal? plyr 3)
        (list (car list1) (cadr list1) (append (list (caaddr list1) (car(cdaddr list1)) (+ (cardId list1 plyr card) (cadr(cdaddr list1)))) (cddr(cdaddr list1))) (append (list (car(cadddr list1))) (list(cadr(cadddr list1))) (list(append (list card) (caddr(cadddr list1)))) (cdddr(cadddr list1))) (cdar(cddddr list1)) (cadr(cddddr list1))))
       ((equal? plyr 4)
        (list (car list1) (cadr list1) (append (list (caaddr list1) (car(cdaddr list1)) (cadr(cdaddr list1)) (+ (cardId list1 plyr card) (caddr(cdaddr list1)))) (list)) (append (list (car(cadddr list1))) (list(cadr(cadddr list1))) (list(caddr(cadddr list1)))(list(append (list card) (cadddr(cadddr list1)))) (list)) (cdar(cddddr list1)) (cadr(cddddr list1))))
       ))

;; ==================================================================================================================================
;; Card Id
;; Identify the card an its value. Return the value of the card. 
;; card: card to identify
;; ==================================================================================================================================
(define (cardId list1 plyr card)
  (cond ((equal? (string-ref card 0) #\A)
         (asCase (caddr list1) plyr))
        ((equal? (string-ref card 0) #\B)
         2)
        ((equal? (string-ref card 0) #\C)
         3)
        ((equal? (string-ref card 0) #\D)
         4)
        ((equal? (string-ref card 0) #\E)
         5)
        ((equal? (string-ref card 0) #\F)
         6)
        ((equal? (string-ref card 0) #\G)
         7)
        ((equal? (string-ref card 0) #\H)
         8)
        ((equal? (string-ref card 0) #\I)
         9)
        (else
         10)
   )
)

;; ==================================================================================================================================
;; As Case
;; Verify the As case to check if player needs the As card to count 1 or 11.
;; pts: list of player points until now
;; plyr: player number who ask for one more card
;; ==================================================================================================================================

(define (asCase pts plyr)
  (cond ((equal? plyr 1)
         (cond ((> (+ (car pts) 11) 21)
                 1)
                (else
                 11)
          ))
        ((equal? plyr 2)
         (cond ((> (+ (cadr pts) 11) 21)
                1)
                (else
                 11)
          ))
        ((equal? plyr 3)
         (cond ((> (+ (caddr pts) 11) 21)
                 1)
                (else
                 11)
          ))
        ((equal? plyr 4)
         (cond ((> (+ (cadddr pts) 11) 21)
                 1)
                (else
                 11)
          ))
   )
)
(provide (all-defined-out))
#lang racket

(define cards '("A1" "B1" "C1" "D1" "E1" "F1" "G1" "H1" "I1" "J1" "K1" "L1" "M1"
                "A2" "B2" "C2" "D2" "E2" "F2" "G1" "H2" "I2" "J2" "K2" "L2" "M2"
                "A3" "B3" "C3" "D3" "E3" "F3" "G3" "H3" "I3" "J3" "K3" "L3" "M3"
                "A4" "B4" "C4" "D4" "E4" "F4" "G4" "H4" "I4" "J4" "K4" "L4" "M4"))

;; (actualizar '("a" "b" "r") '(0 0 0 0))
(define(actualizar list1 listnum)
  (cond((equal? (contar list1) 1) (list (actualizar_aux list1) (list (car listnum) (cadr listnum)) '(0 0) '(("B1" "C1") ("D1" "E1")) (shuffleDeck cards 51) 1))
       ((equal? (contar list1) 2) (list (actualizar_aux list1) (list (car listnum) (cadr listnum) (caddr listnum)) '(0 0 0) '(("B2" "C2") ("D2" "E2") ("F2" "G2")) (shuffleDeck cards 51) 2))
       (else (list (actualizar_aux list1) (list (car listnum) (cadr listnum) (caddr listnum) (cadddr listnum)) '(0 0 0 0) '(("B3" "C3") ("D3" "E3") ("F3" "G3") ("H3" "I3")) (shuffleDeck cards 51) 3))
       ))

(define(actualizar_aux list1)
  (cond((null? list1) '("Crupier"))
       ((equal?  (car list1) "-") (actualizar_aux (cdr list1)))
       (else (cons (car list1) (actualizar_aux (cdr list1))))
       ))

(define(contar list1)
  (cond((null? list1) 0)
       ((equal? (car list1) "-") (+ 0 (contar (cdr list1))))
       ((equal? (car list1) "Crupier") (+ 0 (contar (cdr list1))))
       (else (+ 1 (contar (cdr list1))))
       ))

;; (mayor(caddr(actualizar '("a" "b" "r"))))
(define(mayor lista)
  (cond((null? lista) '())
       (else (mayor_aux (car lista) (cdr lista)))
       ))
(define(mayor_aux ele lista)
  (cond((null? lista) ele)
       ((< ele (car lista)) (mayor_aux (car lista) (cdr lista)))
       (else (mayor_aux ele (cdr lista)))
       ))
(define (eliminate_aux list1 num)
  (cond((equal? num 0) (cdr list1))
       (else (cons (car list1) (eliminate_aux (cdr list1) (- num 1))))
       ))
;;(eliminate (actualizar '("a" "b" "r")) 1)
(define(eliminate list1 num)
  (list (eliminate_aux(car list1) num) (eliminate_aux(cadr list1)num) (eliminate_aux(caddr list1)num) (cadddr list1) (car(cddddr list1)) (cadr(cddddr list1))))

;; (winner '(("Crupier" 17 "No" 1) ("a" 0 "No" 1) ("b" 0 "No" 1) ("r" 0 "No" 1)) 1)
(define(winner list1 num)
  (cond((equal? num 0) list1)
       ((< 21 (cadar list1)) (cons (car list1) (winner (cdr list1) num)))
       (else (cons (list (caar list1) (cadar list1) "Yes" (+ 1 (car(cdddar list1)))) (winner (cdr list1) 0)))
       ))

;; (table (actualizar '("a" "b" "c") '(0 0 0 0)))
(define(table list1)
  (append (list (cadr(cddddr list1))) (winner(table_aux list1 (mayor (caddr list1)))1) (list(car list1))))

(define(table_aux list1 max)
  (cond((null? max) '())
       ((equal? max (caaddr list1)) (cons (list (caar list1) (caaddr list1) "" (caadr list1))
                                          (table_aux (eliminate list1 0) (mayor (eliminate_aux(caddr list1) 0)))))
       ((equal? max (car (cdaddr list1))) (cons(list (cadar list1) (car (cdaddr list1)) "" (cadadr list1))
                                          (table_aux (eliminate list1 1) (mayor (eliminate_aux(caddr list1) 1)))))
       ((equal? max (cadr(cdaddr list1))) (cons(list (caddar list1) (cadr(cdaddr list1)) "" (car(cddadr list1)))
                                          (table_aux (eliminate list1 2) (mayor (eliminate_aux(caddr list1) 2)))))
       ((equal? max (caddr(cdaddr list1))) (cons(list (car(cdddar list1)) (caddr(cdaddr list1)) "" (cadr(cddadr list1)))
                                           (table_aux (eliminate list1 3) (mayor (eliminate_aux(caddr list1) 3)))))
       ))

;; (order 3 '(("Crupier" 17 "Yes" 1) ("a" 0 "" 5) ("b" 0 "" 6) ("c" 0 "" 7) ("a" "b" "c" "Crupier")))

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

;;===================================================================================================================================
;; (cargar (actualizar '("a" "b" "r") '(0 0 0 0)) 1)
(define (cargar list1 num1)
  (cond((equal? num1 1) (list (car list1) (cadr list1) (append (list (+ 3 (caaddr list1))) (cdaddr list1)) (append (list(append (list "H9") (car(cadddr list1)))) (cdr(cadddr list1))) (car(cddddr list1)) (cadr(cddddr list1))))
       ((equal? num1 2) (list (car list1) (cadr list1) (append (list (caaddr list1) (+ 3 (car(cdaddr list1)))) (cdr(cdaddr list1))) (append (list (car(cadddr list1))) (list(append (list "H9") (cadr(cadddr list1)))) (cddr(cadddr list1))) (car(cddddr list1)) (cadr(cddddr list1))))
       ((equal? num1 3) (list (car list1) (cadr list1) (append (list (caaddr list1) (car(cdaddr list1)) (+ 3 (cadr(cdaddr list1)))) (cddr(cdaddr list1))) (append (list (car(cadddr list1))) (list(cadr(cadddr list1))) (list(append (list "H9") (caddr(cadddr list1)))) (cdddr(cadddr list1))) (car(cddddr list1)) (cadr(cddddr list1))))
       ((equal? num1 4) (list (car list1) (cadr list1) (append (list (caaddr list1) (car(cdaddr list1)) (cadr(cdaddr list1)) (+ 3 (caddr(cdaddr list1)))) (list)) (append (list (car(cadddr list1))) (list(cadr(cadddr list1))) (list(caddr(cadddr list1)))(list(append (list "H9") (cadddr(cadddr list1)))) (list)) (car(cddddr list1)) (cadr(cddddr list1))))
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

;; ==================================================================================================================================
;; cardsDeck
;; Shuffle the cards deck and deal them randomly
;; ==================================================================================================================================
(define (cardsDeck)
  (cardsDeck_aux (rndCard cards 52)))

(define (cardsDeck_aux card)
  (cond ((equal? (string-ref card 0) #\A)
         (cardId card "As"))
        ((equal? (string-ref card 0) #\B)
         (cardId card "2"))
        ((equal? (string-ref card 0) #\C)
         (cardId card "3"))
        ((equal? (string-ref card 0) #\D)
         (cardId card "4"))
        ((equal? (string-ref card 0) #\E)
         (cardId card "5"))
        ((equal? (string-ref card 0) #\F)
         (cardId card "6"))
        ((equal? (string-ref card 0) #\G)
         (cardId card "7"))
        ((equal? (string-ref card 0) #\H)
         (cardId card "8"))
        ((equal? (string-ref card 0) #\I)
         (cardId card "9"))
        ((equal? (string-ref card 0) #\J)
         (cardId card "10"))
        ((equal? (string-ref card 0) #\K)
         (cardId card "Jota"))
        ((equal? (string-ref card 0) #\L)
         (cardId card "Quina"))
        ((equal? (string-ref card 0) #\M)
         (cardId card "Ka"))
   )
)

(define (cardId card id)
  (cond ((equal? (string-ref card 1) #\1)
         (write (string-append "¡" id " de corazones!")))
        ((equal? (string-ref card 1) #\2)
         (write (string-append "¡" id " de treboles!")))
        ((equal? (string-ref card 1) #\3)
         (write (string-append "¡" id " de diamantes!")))
        ((equal? (string-ref card 1) #\4)
         (write (string-append "¡" id " de espadas!")))
  )
)


(provide (all-defined-out))
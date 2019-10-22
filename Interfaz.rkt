#lang racket/gui
(require "Logica.rkt")
(require racket/format)
(require racket/draw
         net/url)
;;=================================================================================================================================
(define puntuacion (new dialog% [label "BlaCEkJack"] [width 1000]))

(define title(new horizontal-panel% [parent puntuacion] [stretchable-height #t]))
(define row_first(new horizontal-panel% [parent puntuacion] [stretchable-height #t]))
(define row_second(new horizontal-panel% [parent puntuacion] [stretchable-height #t]))
(define row_third(new horizontal-panel% [parent puntuacion] [stretchable-height #t]))
(define row_quarter(new horizontal-panel% [parent puntuacion] [stretchable-height #t]))
(define buttons(new horizontal-panel% [parent puntuacion] [stretchable-height #t]))

(new message% [parent title] [label "Name"] [font (make-object font% 20 'default)] [stretchable-width #t])
(new message% [parent title] [label "Score"] [font (make-object font% 20 'default)] [stretchable-width #t])
(new message% [parent title] [label "Win"] [font (make-object font% 20 'default)] [stretchable-width #t])
(new message% [parent title] [label "Squall"] [font (make-object font% 20 'default)] [stretchable-width #t])

(define first(new message% [parent row_first] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define first_score(new message% [parent row_first] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define first_w(new message% [parent row_first] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define first_s(new message% [parent row_first] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))

(define second(new message% [parent row_second] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define second_score(new message% [parent row_second] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define second_w(new message% [parent row_second] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define second_s(new message% [parent row_second] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))

(define third(new message% [parent row_third] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define third_score(new message% [parent row_third] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define third_w(new message% [parent row_third] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define third_s(new message% [parent row_third] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))

(define quarter(new message% [parent row_quarter] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define quarter_score(new message% [parent row_quarter] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define quarter_w(new message% [parent row_quarter] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define quarter_s(new message% [parent row_quarter] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))

(new button% [parent buttons] [stretchable-width #t] [label "Play Again"] [callback (lambda (button event)(re_play))])
(new button% [parent buttons] [stretchable-width #t] [label "Finish"] [callback (lambda (button event)(send puntuacion show #f)
                                                                                  (send frame show #f))])

(define(re_play)
  (cond((equal? (car list3) 1) (send bas2 set-label "")
                                         (set! list3 (sets-values (actualizar (cadddr list3) (order 1 (cdr list3))))))
       ((equal? (car list3) 2) (send bas1 set-label "")
                                         (send bas3 set-label "")
                                         (set! list3 (sets-values (actualizar (car(cddddr list3)) (order 2 (cdr list3))))))
       (else (send bas1 set-label "")
             (send bas2 set-label "")
             (send bas3 set-label "")
             (set! list3 (sets-values (actualizar (cadr(cddddr list3)) (order 3 (cdr list3))))))
       )
  (send puntuacion show #f)
  )
(define (set-score list1)
  (cond((equal? (car list1) 1) (send first set-label (caadr list1))
                               (send first_score set-label (~v (cadadr list1)))
                               (send first_w set-label (car(cddadr list1)))
                               (send first_s set-label (~v (cadr(cddadr list1))))
                               (send second set-label (caaddr list1))
                               (send second_score set-label (~v (car(cdaddr list1))))
                               (send second_w set-label (cadr(cdaddr list1)))
                               (send second_s set-label (~v (caddr(cdaddr list1)))))
       ((equal? (car list1) 2) (send first set-label (caadr list1))
                               (send first_score set-label (~v (cadadr list1)))
                               (send first_w set-label (car(cddadr list1)))
                               (send first_s set-label (~v (cadr(cddadr list1))))
                               (send second set-label (caaddr list1))
                               (send second_score set-label (~v (car(cdaddr list1))))
                               (send second_w set-label (cadr(cdaddr list1)))
                               (send second_s set-label (~v (caddr(cdaddr list1))))
                               (send third set-label (car(cadddr list1)))
                               (send third_score set-label (~v (cadr(cadddr list1))))
                               (send third_w set-label (caddr(cadddr list1)))
                               (send third_s set-label (~v (cadddr(cadddr list1)))))
       (else (send first set-label (caadr list1))
             (send first_score set-label (~v (cadadr list1)))
             (send first_w set-label (car(cddadr list1)))
             (send first_s set-label (~v (cadr(cddadr list1))))
             (send second set-label (caaddr list1))
             (send second_score set-label (~v (car(cdaddr list1))))
             (send second_w set-label (cadr(cdaddr list1)))
             (send second_s set-label (~v (caddr(cdaddr list1))))
             (send third set-label (car(cadddr list1)))
             (send third_score set-label (~v (cadr(cadddr list1))))
             (send third_w set-label (caddr(cadddr list1)))
             (send third_s set-label (~v (cadddr(cadddr list1))))
             (send quarter set-label (caar(cddddr list1)))
             (send quarter_score set-label (~v (cadar(cddddr list1))))
             (send quarter_w set-label (caddar(cddddr list1)))
             (send quarter_s set-label (~v (car(cdddar(cddddr list1))))))
       )
  (set! list3 list1)
  (send puntuacion show #t))


;;=================================================================================================================================
(define frame (new frame%
                   [label "BlaCEkJack"]
                   [style '(hide-menu-bar)]
                   ))

(define row1
  (new horizontal-panel%
       [parent frame]
       [style       '(border)]
       [stretchable-height #t]))

(define row2
  (new horizontal-panel%
       [parent frame]
       [stretchable-height #t]))

(define col1
  (new vertical-panel%
       [parent row1]
       [min-width 150]
       [style '(border hide-hscroll)]
       [stretchable-width #f]))
(define col2
  (new vertical-panel%
       [parent row1]
       [style '(border)]
       [stretchable-height #t]))
(define col3
  (new vertical-panel%
       [parent row1]
       [min-width 150]
       [style '(border)]
       [stretchable-width #f]))

(define col4
  (new vertical-panel%
       [parent row2]
       [style '(border)]
       [stretchable-height #t]))
(define col5
  (new vertical-panel%
       [parent row2]
       [style '(border)]
       [stretchable-height #t]))
(define col6
  (new vertical-panel%
       [parent row2]
       [style '(border)]
       [stretchable-height #t]))

(define row3
  (new horizontal-panel%
       [parent col2]
       [style '(border)]
       [min-width 500]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))
(new message% [parent row3] [label "     Crupier"] [font (make-object font% 20 'default)] [stretchable-width #t])
(define pun1(new message% [parent row3] [label "     Puntuacion: 0"] [font (make-object font% 20 'default)] [stretchable-width #t]))

(define list3 '())
(define n1(new message% [parent col1] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define n2(new message% [parent col1] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define n3(new message% [parent col1] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))
(define n4(new message% [parent col1] [label ""] [font (make-object font% 20 'default)] [stretchable-width #t]))

(define row4
  (new horizontal-panel%
       [parent col4]
       [style       '(border)]
       [min-width 250]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))

(define n5 (new message% [parent row4] [label ""] [stretchable-width #t]))
(define pun2(new message% [parent row4] [label ""] [stretchable-width #t]))

(define row5
  (new horizontal-panel%
       [parent col5]
       [style       '(border)]
       [min-width 250]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))

(define n6(new message% [parent row5] [label ""] [stretchable-width #t]))
(define pun3(new message% [parent row5] [label ""] [stretchable-width #t]))

(define row6
  (new horizontal-panel%
       [parent col6]
       [style       '(border)]
       [min-width 250]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))

(define n7(new message% [parent row6] [label ""] [stretchable-width #t]))
(define pun4(new message% [parent row6] [label ""] [stretchable-width #t]))

(define row7
  (new horizontal-panel%
       [parent col4]
       [style       '(border)]
       [min-width 250]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))
(define row8
  (new horizontal-panel%
       [parent col5]
       [style       '(border)]
       [min-width 250]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))
(define row9
  (new horizontal-panel%
       [parent col6]
       [style       '(border)]
       [min-width 250]	 
       [min-height 50]
       [stretchable-height #f]
       [stretchable-width #f]))

(define (crupier_turn list1)
  (cond((equal? (cadr (cddddr list1)) 1) (cond((>= (car(cdaddr list1)) 17) (set-score (table list1)))
                                              (else (sleep 1.5)
                                                    (crupier_turn (sets-values(cargar list1 2))))
                                              ))
       ((equal? (cadr (cddddr list1)) 2) (cond((>= (cadr(cdaddr list1)) 17) (set-score (table list1)))
                                              (else (sleep 1.5)
                                                    (crupier_turn (sets-values(cargar list1 3))))
                                              ))
       ((equal? (cadr (cddddr list1)) 3) (cond((>= (caddr(cdaddr list1)) 17) (set-score (table list1)))
                                              (else (sleep 1.5)
                                                    (crupier_turn (sets-values(cargar list1 4))))
                                              ))
       ))

(define bas1(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))
(define bas2(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))
(define bas3(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))

(define logo (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TK.png"))))
(define logo1 (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/TK(1).png"))))


(define (sets-carts list1 list2 num1)
  (cond((equal? num1 1) (cond((null? list2) list2)
                             (else (send (car list1) set-label logo)
                                   (sets-carts (cdr list1) (cdr list2) num1))))
       ((equal? num1 2) (cond((null? list2) list2)
                             (else (send (car list1) set-label logo1)
                                   (sets-carts (cdr list1) (cdr list2) num1))))
       ))

(define (new_rond list1)
  (cond((and (or (equal? (send bas1 get-label) "0") (equal? (send bas1 get-label) "2")) (or (equal? (send bas2 get-label) "0") (equal? (send bas2 get-label) "2")) (or (equal? (send bas3 get-label) "0") (equal? (send bas3 get-label) "2")))
        (cond((equal? (send bas1 get-label) "2") (cond((>= (caaddr list1) 21) (send bas1 set-label "0"))
                                                      (else (send bas1 set-label ""))
                                                      ))
             )
        (cond((equal? (send bas2 get-label) "2") (cond((equal? (cadr (cddddr list1)) 1) (cond((>= (caaddr list1) 21) (send bas2 set-label "0"))
                                                                                             (else (send bas2 set-label ""))
                                                                                             ))
                                                      ((equal? (cadr (cddddr list1)) 3) (cond((>= (car(cdaddr list1)) 21) (send bas2 set-label "0"))
                                                                                             (else (send bas2 set-label ""))
                                                                                             ))
                                                ))
             )
        (cond((equal? (send bas3 get-label) "2") (cond((equal? (cadr (cddddr list1)) 2) (cond((>= (car(cdaddr list1)) 21) (send bas3 set-label "0"))
                                                                                             (else (send bas3 set-label ""))
                                                                                             ))
                                                      ((equal? (cadr (cddddr list1)) 3) (cond((>= (cadr(cdaddr list1)) 21) (send bas3 set-label "0"))
                                                                                             (else (send bas3 set-label ""))
                                                                                             ))
                                                ))
             )
        (cond((and (equal? (send bas1 get-label) "0") (equal? (send bas2 get-label) "0") (equal? (send bas3 get-label) "0"))
              (crupier_turn list1))
             ))
       ((equal? (send bas1 get-label) "1") (send bas1 set-label "2")
                                           (set! list3 (sets-values (cargar list1 1))))
       ((equal? (send bas2 get-label) "1") (send bas2 set-label "2")
                                           (cond((equal? (cadr (cddddr list1)) 1) (set! list3 (sets-values (cargar list1 1))))
                                                ((equal? (cadr (cddddr list1)) 3) (set! list3 (sets-values (cargar list1 2))))
                                                ))
       ((equal? (send bas3 get-label) "1") (send bas3 set-label "2")
                                           (cond((equal? (cadr (cddddr list1)) 2) (set! list3 (sets-values (cargar list1 2))))
                                                ((equal? (cadr (cddddr list1)) 3) (set! list3 (sets-values (cargar list1 3))))
                                                ))
       )list3)

(define(sets-values-i list1)
  (cond((equal? (cadr (cddddr list1)) 1) (send bas1 set-label "0")
                                         (send bas3 set-label "0")
                                         (send row8 add-child p8)
                                         (send row8 add-child q8)
                                         (send n1 set-label (caar list1))
                                         (send n2 set-label "Crupier")
                                         (send n6 set-label (caar list1))
                                         (send pun3 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (car(cdaddr list1)))))
                                         (sets-carts (list c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24) (car(cadddr list1)) 1)
                                         (sets-carts (list c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) (cadr(cadddr list1)) 2))
       ((equal? (cadr (cddddr list1)) 2) (send bas2 set-label "0")
                                         (send row7 add-child p7)
                                         (send row7 add-child q7)
                                         (send row9 add-child p9)
                                         (send row9 add-child q9)
                                         (send n1 set-label (caar list1))
                                         (send n2 set-label (cadar list1))
                                         (send n3 set-label "Crupier")
                                         (send n5 set-label (caar list1))
                                         (send n7 set-label (cadar list1))
                                         (send pun2 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun4 set-label (string-append "Puntuacion: " (~v (car (cdaddr list1)))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (cadr (cdaddr list1)))))
                                         (sets-carts (list c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12) (car(cadddr list1)) 1)
                                         (sets-carts (list c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36) (cadr(cadddr list1)) 1)
                                         (sets-carts (list c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) (caddr(cadddr list1)) 2))
       (else (send row7 add-child p7)
             (send row7 add-child q7)
             (send row8 add-child p8)
             (send row8 add-child q8)
             (send row9 add-child p9)
             (send row9 add-child q9)
             (send n1 set-label (caar list1))
             (send n2 set-label (cadar list1))
             (send n3 set-label (caddar list1))
             (send n4 set-label "Crupier")
             (send n5 set-label (caar list1))
             (send n6 set-label (cadar list1))
             (send n7 set-label (caddar list1))
             (send pun2 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
             (send pun3 set-label (string-append "Puntuacion: " (~v (car (cdaddr list1)))))
             (send pun4 set-label (string-append "Puntuacion: " (~v (cadr (cdaddr list1)))))
             (send pun1 set-label (string-append "     Puntuacion: " (~v (caddr (cdaddr list1)))))
             (sets-carts (list c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12) (car(cadddr list1)) 1)
             (sets-carts (list c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24) (car(cadddr list1)) 1)
             (sets-carts (list c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36) (cadr(cadddr list1)) 1)
             (sets-carts (list c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) (cadddr(cadddr list1)) 2))
       )list1)

(define(sets-values list1)
  (cond((equal? (cadr (cddddr list1)) 1) (send pun3 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (car(cdaddr list1)))))
                                         (sets-carts (list c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24) (car(cadddr list1)) 1)
                                         (sets-carts (list c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) (cadr(cadddr list1)) 2))
       ((equal? (cadr (cddddr list1)) 2) (send pun2 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun4 set-label (string-append "Puntuacion: " (~v (car (cdaddr list1)))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (cadr (cdaddr list1)))))
                                         (sets-carts (list c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12) (car(cadddr list1)) 1)
                                         (sets-carts (list c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36) (cadr(cadddr list1)) 1)
                                         (sets-carts (list c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) (caddr(cadddr list1)) 2))
       (else (send pun2 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
             (send pun3 set-label (string-append "Puntuacion: " (~v (car (cdaddr list1)))))
             (send pun4 set-label (string-append "Puntuacion: " (~v (cadr (cdaddr list1)))))
             (send pun1 set-label (string-append "     Puntuacion: " (~v (caddr (cdaddr list1)))))
             (sets-carts (list c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12) (car(cadddr list1)) 1)
             (sets-carts (list c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24) (car(cadddr list1)) 1)
             (sets-carts (list c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36) (cadr(cadddr list1)) 1)
             (sets-carts (list c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) (cadddr(cadddr list1)) 2))
       )list1)

(new button% [parent col3]
             [label "Salir"]
             [callback (lambda (button event)
                         (send frame show #f))])

(define p7(new button% [parent row7]
             [label "Pedir"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas1 get-label)) (send bas1 set-label "1")
                                                                 (new_rond(new_rond list3)))))]))
(define q7(new button% [parent row7]
             [label "Quedar"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas1 get-label)) (send bas1 set-label "0")
                                                                 (new_rond list3))))]))

(define p8(new button% [parent row8]
             [label "Pedir"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas2 get-label)) (send bas2 set-label "1")
                                                                 (new_rond(new_rond list3)))))]))
(define q8(new button% [parent row8]
             [label "Quedar"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas2 get-label)) (send bas2 set-label "0")
                                                                 (new_rond list3))))]))

(define p9(new button% [parent row9]
             [label "Pedir"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas3 get-label)) (send bas3 set-label "1")
                                                                 (new_rond(new_rond list3)))))]))
(define q9(new button% [parent row9]
             [label "Quedar"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas3 get-label)) (send bas3 set-label "0")
                                                                 (new_rond list3))))]))

(define row10
  (new horizontal-panel%
       [parent col4]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define row20
  (new horizontal-panel%
       [parent col4]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define row11
  (new horizontal-panel%
       [parent col5]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define row21
  (new horizontal-panel%
       [parent col5]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))


(define row13
  (new horizontal-panel%
       [parent col6]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define row23
  (new horizontal-panel%
       [parent col6]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define row14
  (new horizontal-panel%
       [parent col2]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define row24
  (new horizontal-panel%
       [parent col2]
       [style       '(border)]
       [stretchable-height #t]
       [stretchable-width #t]))

(define empty (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/E.png"))))
(define empty1 (read-bitmap (get-pure-port (string->url "file:////home/samuel/Escritorio/Cursos/Lenguajes/Funcional/DrRacket/Images/E(1).png"))))

;; Cartas 1
(define c1(new message%  [parent row10] [label empty][stretchable-width #t]))
(define c2(new message%  [parent row10] [label empty][stretchable-width #t]))
(define c3(new message%  [parent row10] [label empty][stretchable-width #t]))
(define c4(new message%  [parent row10] [label empty][stretchable-width #t]))
(define c5(new message%  [parent row10] [label empty][stretchable-width #t]))
(define c6(new message%  [parent row10] [label empty][stretchable-width #t]))
(define c7(new message%  [parent row20] [label empty][stretchable-width #t]))
(define c8(new message%  [parent row20] [label empty][stretchable-width #t]))
(define c9(new message%  [parent row20] [label empty][stretchable-width #t]))
(define c10(new message% [parent row20] [label empty][stretchable-width #t]))
(define c11(new message% [parent row20] [label empty][stretchable-width #t]))
(define c12(new message% [parent row20] [label empty][stretchable-width #t]))

;; Cartas 2
(define c13(new message% [parent row11] [label empty][stretchable-width #t]))
(define c14(new message% [parent row11] [label empty][stretchable-width #t]))
(define c15(new message% [parent row11] [label empty][stretchable-width #t]))
(define c16(new message% [parent row11] [label empty][stretchable-width #t]))
(define c17(new message% [parent row11] [label empty][stretchable-width #t]))
(define c18(new message% [parent row11] [label empty][stretchable-width #t]))
(define c19(new message% [parent row21] [label empty][stretchable-width #t]))
(define c20(new message% [parent row21] [label empty][stretchable-width #t]))
(define c21(new message% [parent row21] [label empty][stretchable-width #t]))
(define c22(new message% [parent row21] [label empty][stretchable-width #t]))
(define c23(new message% [parent row21] [label empty][stretchable-width #t]))
(define c24(new message% [parent row21] [label empty][stretchable-width #t]))

;; Cartas 3
(define c25(new message% [parent row13] [label empty][stretchable-width #t]))
(define c26(new message% [parent row13] [label empty][stretchable-width #t]))
(define c27(new message% [parent row13] [label empty][stretchable-width #t]))
(define c28(new message% [parent row13] [label empty][stretchable-width #t]))
(define c29(new message% [parent row13] [label empty][stretchable-width #t]))
(define c30(new message% [parent row13] [label empty][stretchable-width #t]))
(define c31(new message% [parent row23] [label empty][stretchable-width #t]))
(define c32(new message% [parent row23] [label empty][stretchable-width #t]))
(define c33(new message% [parent row23] [label empty][stretchable-width #t]))
(define c34(new message% [parent row23] [label empty][stretchable-width #t]))
(define c35(new message% [parent row23] [label empty][stretchable-width #t]))
(define c36(new message% [parent row23] [label empty][stretchable-width #t]))

;; Cartas Crupier
(define c37(new message% [parent row14] [label empty1][stretchable-width #t]))
(define c38(new message% [parent row14] [label empty1][stretchable-width #t]))
(define c39(new message% [parent row14] [label empty1][stretchable-width #t]))
(define c40(new message% [parent row14] [label empty1][stretchable-width #t]))
(define c41(new message% [parent row14] [label empty1][stretchable-width #t]))
(define c42(new message% [parent row14] [label empty1][stretchable-width #t]))
(define c43(new message% [parent row24] [label empty1][stretchable-width #t]))
(define c44(new message% [parent row24] [label empty1][stretchable-width #t]))
(define c45(new message% [parent row24] [label empty1][stretchable-width #t]))
(define c46(new message% [parent row24] [label empty1][stretchable-width #t]))
(define c47(new message% [parent row24] [label empty1][stretchable-width #t]))
(define c48(new message% [parent row24] [label empty1][stretchable-width #t]))


;; ==============================================================================================================================

(define dialog (instantiate dialog% ("BlaCEkJack")))

(define tfield1(new text-field% [parent dialog] [label "Ingrese nombre de usuario1"]))
(define tfield2(new text-field% [parent dialog] [label "Ingrese nombre de usuario2"][init-value "-"]))
(define tfield3(new text-field% [parent dialog] [label "Ingrese nombre de usuario3"][init-value "-"]))

(define panel (new horizontal-panel% [parent dialog]
                                     [alignment '(center center)]))

(new button%
     [parent panel]
     [label "Cancel"]
     [callback (lambda (button event)
                 (send tfield1 set-value "")
                 (send tfield2 set-value "-")
                 (send tfield2 set-value "-")
                 )]
     )

;; estudiar lambda
(define(error)
  (send tfield1 set-value "error")
  (send tfield2 set-value "error")
  (send tfield3 set-value "error")
  (sleep 1)
  (send tfield1 set-value "")
  (send tfield2 set-value "-")
  (send tfield3 set-value "-")
  )

(define(bCEj list1)
  (send frame show #t)
  (send dialog show #f)
  (set! list3 (sets-values-i (actualizar list1 (list 0 0 0 0))))
  )

(define(verificar)
  (cond((equal? (send tfield1 get-value) "") #t)
       ((equal? (send tfield2 get-value) "") #t)
       ((equal? (send tfield3 get-value) "") #t)
       ((equal? (send tfield1 get-value) "Crupier") #t)
       ((equal? (send tfield2 get-value) "Crupier") #t)
       ((equal? (send tfield3 get-value) "Crupier") #t)
       ((and (equal? (send tfield1 get-value) "-") (equal? (send tfield2 get-value) "-") (equal? (send tfield3 get-value) "-")) #t)
       (else #f)
       )
  )

(new button%
     [parent panel]
     [label "Ok"]
     [callback (lambda (button event)
                 (cond((equal? (verificar) #t) (error))
                      (else (bCEj (list (send tfield1 get-value) (send tfield2 get-value) (send tfield3 get-value)))))
                 )]
     )
 
(send dialog show #t)

;;==============================================================================================================================

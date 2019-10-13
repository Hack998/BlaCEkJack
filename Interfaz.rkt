#lang racket/gui
(require "Logica.rkt")
(require racket/format)
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
  (cond((equal? (cadr (cddddr list3)) 1) (send bas2 set-label ""))
       ((equal? (cadr (cddddr list3)) 2) (send bas1 set-label "")
                                         (send bas3 set-label ""))
       (else (send bas1 set-label "")
             (send bas2 set-label "")
             (send bas3 set-label ""))
       )
  (send puntuacion show #f)
  (set! list3 (sets-values (actualizar (car list3)))))

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
                                              (else (send n1 set-label "wins 1"))
                                              ))
       ((equal? (cadr (cddddr list1)) 2) (cond((>= (cadr(cdaddr list1)) 17) (set-score (table list1)))
                                              (else (send n1 set-label "wins 2"))
                                              ))
       ((equal? (cadr (cddddr list1)) 3) (cond((>= (caddr(cdaddr list1)) 17) (set-score (table list1)))
                                              (else (send n1 set-label "wins 3"))
                                              ))
       ))

(define bas1(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))
(define bas2(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))
(define bas3(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))

(define (new_rond list1)
  (cond((and (or (equal? (send bas1 get-label) "0") (equal? (send bas1 get-label) "2")) (or (equal? (send bas2 get-label) "0") (equal? (send bas2 get-label) "2")) (or (equal? (send bas3 get-label) "0") (equal? (send bas3 get-label) "2")))
        (cond((equal? (send bas1 get-label) "2") (send bas1 set-label ""))
             )
        (cond((equal? (send bas2 get-label) "2") (send bas2 set-label ""))
             )
        (cond((equal? (send bas3 get-label) "2") (send bas3 set-label ""))
             )
        (cond((and (equal? (send bas1 get-label) "0") (equal? (send bas2 get-label) "0") (equal? (send bas3 get-label) "0"))
              (crupier_turn list1))
             ))
       ((equal? (send bas1 get-label) "1") (send bas1 set-label "2")
                                           (sets-values list1))
       ((equal? (send bas2 get-label) "1") (send bas2 set-label "2")
                                           (sets-values list1))
       ((equal? (send bas3 get-label) "1") (send bas3 set-label "2")
                                           (sets-values list1))
       ))

(define(sets-values-i list1)
  (cond((equal? (cadr (cddddr list1)) 1) (send bas1 set-label "0")
                                         (send bas3 set-label "0")
                                         (send row8 add-child p8)
                                         (send row8 add-child q8)
                                         (send n1 set-label (caar list1))
                                         (send n2 set-label "Crupier")
                                         (send n6 set-label (caar list1))
                                         (send pun3 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (car(cdaddr list1))))))
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
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (cadr (cdaddr list1))))))
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
             (send pun1 set-label (string-append "     Puntuacion: " (~v (caddr (cdaddr list1))))))
       )list1)

(define(sets-values list1)
  (cond((equal? (cadr (cddddr list1)) 1) (send pun3 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (car(cdaddr list1))))))
       ((equal? (cadr (cddddr list1)) 2) (send pun2 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun4 set-label (string-append "Puntuacion: " (~v (car (cdaddr list1)))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (cadr (cdaddr list1))))))
       (else (send pun2 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
             (send pun3 set-label (string-append "Puntuacion: " (~v (car (cdaddr list1)))))
             (send pun4 set-label (string-append "Puntuacion: " (~v (cadr (cdaddr list1)))))
             (send pun1 set-label (string-append "     Puntuacion: " (~v (caddr (cdaddr list1))))))
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
  (set! list3 (sets-values-i (actualizar list1)))
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

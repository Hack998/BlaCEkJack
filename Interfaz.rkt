#lang racket/gui
(require "Logica.rkt")
(require racket/format)
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

(define bas1(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))
(define bas2(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))
(define bas3(new message% [parent col3] [label ""] [stretchable-width #t])) ;[style '(deleted)]))

(define (new_rond list1)
  (cond((equal? (cadr (cddddr list1)) 1) (send n1 set-label "1"))
       ((equal? (cadr (cddddr list1)) 2) (send n1 set-label "2"))
       (else (send n1 set-label "3"))
       ))

(define(sets-values list1)
  (cond((equal? (cadr (cddddr list1)) 1) (send row8 add-child p8)
                                         (send row8 add-child q8)
                                         (send n1 set-label (caar list1))
                                         (send n2 set-label "Crupier")
                                         (send n6 set-label (caar list1))
                                         (send pun3 set-label (string-append "Puntuacion: " (~v (caaddr list1))))
                                         (send pun1 set-label (string-append "     Puntuacion: " (~v (car(cdaddr list1))))))
       ((equal? (cadr (cddddr list1)) 2) (send row7 add-child p7)
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

(new button% [parent col3]
             [label "Salir"]
             [callback (lambda (button event)
                         (send frame show #f))])

(define p7(new button% [parent row7]
             [label "Pedir"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas1 get-label)) (send bas1 set-label "1"))))]))
(define q7(new button% [parent row7]
             [label "Quedar"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas1 get-label)) (send bas1 set-label "0"))))]))

(define p8(new button% [parent row8]
             [label "Pedir"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas2 get-label)) (send bas2 set-label "1"))))]))
(define q8(new button% [parent row8]
             [label "Quedar"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas2 get-label)) (send bas2 set-label "0"))))]))

(define p9(new button% [parent row9]
             [label "Pedir"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas3 get-label)) (send bas3 set-label "1"))))]))
(define q9(new button% [parent row9]
             [label "Quedar"]
             [stretchable-width #t]
             [style '(deleted)]
             [callback (lambda (button event)
                         (cond((equal? "" (send bas3 get-label)) (send bas3 set-label "0"))))]))


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
  (new_rond(sets-values (actualizar list1)))
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

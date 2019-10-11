#lang racket/gui
(require "Logica.rkt")
(define frame (new frame%
                   [label "BlaCEkJack"]
                   [style '(hide-menu-bar)]
                   ))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))

(new button% [parent frame]
             [label "Click Me"]
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])
(new button% [parent frame]
             [label "Salir"]
             [callback (lambda (button event)
                         (send frame show #f))])


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

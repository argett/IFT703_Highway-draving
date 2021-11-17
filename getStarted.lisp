(clear-all)

(define-model conductor
    
(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)

;; ---------- Add Chunk-types here ----------

(chunk-type car id weight)
(chunk-type position id positionX positionY)
(chunk-type speed id vitesse)

(chunk-type turn X_relative_position)
(chunk-type brake power)
(chunk-type other_car relative_speed)
(chunk-type accident relative_speed)

; Addition
; sera utile pour calculer le nombre de voitures autour du conducteur
(chunk-type count-order first second)
(chunk-type add arg1 arg2 sum count)

;; ---------- Add Chunks here ----------

(add-dm
; exemple de notre voiture
 (voiture isa car id 0 weight w)
 (voiture_p isa position id 0 positionX x positionY y)
 (voiture_s isa speed id 0 vitesse s)

; exemple d'une voiture accidenté pour le scénario de base
 (camion isa car id 1 weight nil)
 (camion_p isa position id 1 positionX x positionY y)
 (camion_s isa speed id 1 vitesse s)

 (goal isa accident positionX nil positionY 0 relative_speed nil)

 (brakeSoft isa brake power 1)
 (brakeHard isa brake power 3)

 (turnR isa turn X_relative_position 1) 
 (turnL isa turn X_relative_position -1)

 ; addition

   (a ISA count-order first 0 second 1)
   (b ISA count-order first 1 second 2)
   (c ISA count-order first 2 second 3)
   (d ISA count-order first 3 second 4)
   (e ISA count-order first 4 second 5)
   (f ISA count-order first 5 second 6)
   (g ISA count-order first 6 second 7)
   (h ISA count-order first 7 second 8)
   (i ISA count-order first 8 second 9)
   (j ISA count-order first 9 second 10)
 )

 
;; ---------- Add productions here ----------

;;;;;;;;;;;; Turns ;;;;;;;;;;;;

(p turnL
  =goal>
    ISA turn
    X_relative_position -1
    X_relative_position =deviation
   =retrieval>
      ISA         my_car
      positionX   =x_pos_car
==>
  =goal>
    ISA my_car
    positionX     =x_pos_car + deviation ; à faire en LISP et non par ACTR car ce n'est pas le conducteur qui calcule la vitesse de la voiture
)

(p turnR
  =goal>
    ISA turn
    X_relative_position 1
    X_relative_position =deviation
   =retrieval>
      ISA         my_car
      positionX   =x_pos_car
==>
  =goal>
    ISA my_car
    positionX     =x_pos_car + deviation ; de même
)

;;;;;;;;;;;; Brakes ;;;;;;;;;;;;

(p brakeSoft
   =goal>
    ISA       brake
    power     1
    power     =puissance_de_freinage
  =retrieval>
    ISA       speed
    id        0
    vitesse   =s
==>
  =goal>
   ; ???
  =retrieval>
    ISA speed
    id        0
    vitesse   =s-puissance_de_freinage
)

(p brakeHard
   =goal>
    ISA       brake
    power     3
    power     =puissance_de_freinage
  =retrieval>
    ISA       speed
    id        0
    vitesse   =s
==>
  =goal>
   ; ???
  =retrieval>
    ISA speed
    id        0
    vitesse   =s-puissance_de_freinage
)

;;;;;;;;;;;; Take info ;;;;;;;;;;;;

(p lookLeft
   =goal>
    ISA       a
    a         a
  =retrieval>
    ISA       a
    a         a
    a         a
==>
  =goal>
   ; ???
  =retrieval>
    ISA speed
    a         a
    a         a
)


;;;;;;;;;;;; Addition ;;;;;;;;;;;;

(P initialize-addition
   =goal>
      ISA         add
      arg1        =num1
      arg2        =num2
      sum         nil
  ==>
   =goal>
      ISA         add
      sum         =num1
      count       0
   +retrieval>
      ISA        count-order
      first      =num1
)

(P terminate-addition
   =goal>
      ISA         add
      count       =num
      arg2        =num
      sum         =answer
  ==>
   =goal>
      ISA         add
      count       nil
   !output!       =answer
)

(P increment-count
   =goal>
      ISA         add
      sum         =sum
      count       =count
   =retrieval>
      ISA         count-order
      first       =count
      second      =newcount
  ==>
   =goal>
      ISA         add
      count       =newcount
   +retrieval>
      ISA        count-order
      first      =sum
)

(P increment-sum
   =goal>
      ISA         add
      sum         =sum
      count       =count
    - arg2        =count
   =retrieval>
      ISA         count-order
      first       =sum
      second      =newsum
  ==>
   =goal>
      ISA         add
      sum         =newsum
   +retrieval>
      ISA        count-order
      first      =count
)

;(etablir un premier goal focus pour le démarrage second-goal)
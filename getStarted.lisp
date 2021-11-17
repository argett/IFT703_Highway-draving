(clear-all)

(define-model tutor-model
    
(sgp :esc t :lf .05 :trace-detail medium)

;; Add Chunk-types here

(chunk-type car id weight)
(chunk-type position id positionX positionY)
(chunk-type speed id vitesse)

(chunk-type turn X_relative_position)
(chunk-type brake power)
(chunk-type other_car relative_speed)
(chunk-type accident relative_speed)

;; Add Chunks here

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

 (brakeSoft isa brake power 2)
 (brakeHard isa brake power 5)

 (turnR isa turn X_relative_position 1) 
 (turnL isa turn X_relative_position -1)
 )

 
;; Add productions here

; Turns

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
    positionX     =x_pos_car + deviation
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
    positionX     =x_pos_car + deviation
)

; Brakes

(p brakeSoft
   =goal>
    ISA       brake
    power     2
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
    power     5
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

; Take info

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
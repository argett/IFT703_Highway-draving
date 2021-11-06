(clear-all)

(define-model tutor-model
    
(sgp :esc t :lf .05 :trace-detail medium)

;; Add Chunk-types here

(chunk-type my_car weight positionX positionY speed)
(chunk-type turn X_relative_position)
(chunk-type brake power)
(chunk-type other_car positionX positionY relative_speed)
(chunk-type accident positionX positionY relative_speed)

;; Add Chunks here

(add-dm
 (voiture isa my_car weight w positionX x positionY y speed s)

 (goal isa accident positionX nil positionY 0 relative_speed nil)

 (brakeSoft isa brake power 2)
 (brakeHard isa brake power 5)

 (turnR isa turn X_relative_position 1) 
 (turnL isa turn X_relative_position -1)
 )

 
;; Add productions here

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





(p brake
   =goal>
    ISA tooFast
    one-ans busy
    one1 =num1
    one2 =num2
  =retrieval>
    ISA addition-fact
    addend1 =num1
    addend2 =num2
    sum =sum
==>
  =goal>
    one-ans =sum
    carry busy
  +retrieval>
    ISA addition-fact
    addend1 10
    sum =sum
)
   
(p brake
  =goal>
    ISA brakeEasy or brakeHard ; peu importe le chunk on veut freiners, est-cepossible de mettre 'OR' ?
    one-ans busy
    one1 =num1
    one2 =num2
  =retrieval>
    ISA addition-fact
    addend1 =num1
    addend2 =num2
    sum =sum
==>
  =goal>
    one-ans =sum
    carry busy
  +retrieval>
    ISA addition-fact
    addend1 10
    sum =sum
)
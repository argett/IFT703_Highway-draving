(clear-all)

(define-model tutor-model
    
(sgp :esc t :lf .05 :trace-detail medium)

;; Add Chunk-types here

(chunk-type my_car weight positionX positionY speed)
(chunk-type other_car positionX positionY relative_speed)
(chunk-type accident positionX positionY relative_speed)
(chunk-type turn X_relative_position)

;; Add Chunks here

(add-dm
 (goal isa accident positionX nil positionY 0 relative_speed nil)
 (brakeEasy isa my_car weight nil positionX nil positionY nil speed -2)
 (brakeHard isa my_car weight nil positionX nil positionY nil speed -5)

 (turnR isa turn X_relative_position 1)
 (turnL isa turn X_relative_position -1)
 ; ou
 (turnR isa my_car positionX 1 positionY nil relative_speed nil)
 (turnL isa my_car positionX -1 positionY nil relative_speed nil)
 )

 
;; Add productions here


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
    sum =sum)
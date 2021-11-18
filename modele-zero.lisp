(clear-all)

(define-model conductor
    
(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)

;; ---------- Add Chunk-types here ----------

(chunk-type car id weight state)
(chunk-type position id positionX positionY)
(chunk-type speed id vitesse)

(chunk-type turn xRelativePosition)
(chunk-type brake power)
(chunk-type otherCar relativeSpeed)
(chunk-type accident relativeSpeed)


(chunk-type count-speed first second)

;; Addition
;; sera utile pour calculer le nombre de voitures autour du conducteur
(chunk-type add-order first second)
(chunk-type add arg1 arg2 sum count)
;; Soustraction
(chunk-type rem-order first second)
(chunk-type substract arg1 arg2 res)

;; ---------- Add Chunks here ----------

(add-dm
;; exemple de notre voiture
 (voiture isa car id 0 weight w)
 (voitureP isa position id 0 positionX x positionY y)
 (voitureS isa speed id 0 vitesse s)

;; exemple d'une voiture accidenté pour le scénario de base
 (camion isa car id 1 weight nil)
 (camionP isa position id 1 positionX x positionY y)
 (camionS isa speed id 1 vitesse s)

 (brakeSoft isa brake power 1)
 (brakeHard isa brake power 3)

 (turnR isa turn xRelativePosition 1) 
 (turnL isa turn xRelativePosition -1)

;; changement de vitesse

   (v1 ISA add-speed first rapide second moyen)
   (v2 ISA add-speed first moyen second lent)

;; addition
      ; servir a compter les voiture et tourner

   (c0 ISA add-order first -1 second 0)
   (c1 ISA add-order first 0 second 1)
   (c2 ISA add-order first 1 second 2)
   (c3 ISA add-order first 2 second 3)
   (c4 ISA add-order first 3 second 4)
   (c5 ISA add-order first 4 second 5)
   (c6 ISA add-order first 5 second 6)
   (c7 ISA add-order first 6 second 7)
   (c8 ISA add-order first 7 second 8)
   (c9 ISA add-order first 8 second 9)
   
   ; soustaction (l'autoroute est limitée à 3 voies)
   ; tourner

   (r1 ISA rem-order first 0 second -1)
   (r2 ISA rem-order first 1 second 0)

   (detect isa chunk)
 )

 
;; ---------- Add productions here ----------

;;;;;;;;;;;; Turns ;;;;;;;;;;;; 
(p turnL
   =goal>
      ISA                   turn
      xRelativePosition    -1
      xRelativePosition    =deviation
   =retrieval>
      ISA                   my_car
      positionX             =x_pos_car
==>
   =goal>
      ISA                   position
      id                    =0
   =retrieval>
      ISA                   rem-order
      first                 =x_pos_car 
)

(p turnedLeft
   =goal>
      ISA                  position
      id                   0
      positionX            =posX
      positionY            =posY
   =retrieval>
      ISA                  rem-order
      first                posX
      second               =finalPosX

==>
   =goal>
      ISA                  car
      id                   =0
      positionX            =finalPosX
   +imaginal>     ;; Réécriture du chunk position avec id=0
      ISA                 position
      id                  =0
      positionX           =finalPosX
      positionY           =posY
)

(p turnR
   =goal>
      ISA                  turn
      xRelativePosition    1
      xRelativePosition    =deviation
   =retrieval>
      ISA                   my_car
      positionX             =x_pos_car
==>
   =goal>
      ISA                   position
      id                    =0
   =retrieval>
      ISA                   rem-order
      first                 =x_pos_car 
)

(p turnedRight
   =goal>
      ISA                  position
      id                   0
      positionX            =posX
      positionY            =posY
   =retrieval>
      ISA                  rem-order
      first                posX
      second               =finalPosX

==>
   =goal>
      ISA                  car
      id                   =0
      positionX            =finalPosX
   +imaginal>     ;; On attendant de savoir comment réécrire par dessus le chunk position avec id=0
      ISA                 position
      id                  =0
      positionX           =finalPosX
      positionY           =posY
)

;;;;;;;;;;;; Brakes ;;;;;;;;;;;;

;; A changer 

(p brakeSoft
   =goal>
    ISA                   brake
    power                 1
    power                 =puissance_de_freinage
  =retrieval>
    ISA                   speed
    id                    0
    vitesse               =s
==>
  =goal>
   ; ???
  =retrieval>
    ISA speed
    id                     0
    vitesse                =s-puissance_de_freinage
)

;; A changer 

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
    ISA                 a
    a                   a
  =retrieval>
    ISA                 a
    a                   a
    a                   a
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
      ISA        add-order
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
      ISA         add-order
      first       =count
      second      =newcount
  ==>
   =goal>
      ISA         add
      count       =newcount
   +retrieval>
      ISA        add-order
      first      =sum
)

(P increment-sum
   =goal>
      ISA         add
      sum         =sum
      count       =count
    - arg2        =count
   =retrieval>
      ISA         add-order
      first       =sum
      second      =newsum
  ==>
   =goal>
      ISA         add
      sum         =newsum
   +retrieval>
      ISA        add-order
      first      =count
)

;etablir un premier goal focus pour le démarrage second-goal
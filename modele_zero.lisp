(clear-all)

(define-model conductor
    
;;(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)
(sgp :esc t :lf .05)

;; ---------- Add Chunk-types here ----------

(chunk-type car id weight state)
(chunk-type position id positionX positionY)
(chunk-type speed id vitesse)
(chunk-type changeSpeed old new)

(chunk-type turn xRelativePosition)
(chunk-type brake power)
(chunk-type otherCar relativeSpeed)


(chunk-type count-speed first second)

;; Addition
;; sera utile pour calculer le nombre de voitures autour du conducteur
(chunk-type add-order low high)
(chunk-type add arg1 arg2 sum count)
;; Soustraction
(chunk-type rem-order over under)
(chunk-type substract arg1 arg2 res)

;; ---------- Add Chunks here ----------

(add-dm
;; exemple de notre voiture
 (voiture isa car id 0 weight "w")
 (voitureP isa position id 0 positionX "x" positionY "y")
 (voitureS isa speed id 0 vitesse "s")

;; exemple d'une voiture accidenté pour le scénario de base
 (camion isa car id 1 weight nil)
 (camionP isa position id 1 positionX "x" positionY "y")
 (camionS isa speed id 1 vitesse "s")

 (brakeSoft isa brake power 1)
 (brakeHard isa brake power 3)

 (turnR isa turn xRelativePosition 1) 
 (turnL isa turn xRelativePosition -1)

;; changement de vitesse

   (v1 ISA changeSpeed old "rapide" new "moyen")
   (v2 ISA changeSpeed old "moyen" new "lent")

;; addition
   ; servir a compter les voiture et tourner

   (c0 ISA add-order low -1 high 0)
   (c1 ISA add-order low 0 high 1)
   (c2 ISA add-order low 1 high 2)
   (c3 ISA add-order low 2 high 3)
   (c4 ISA add-order low 3 high 4)
   (c5 ISA add-order low 4 high 5)
   (c6 ISA add-order low 5 high 6)
   (c7 ISA add-order low 6 high 7)
   (c8 ISA add-order low 7 high 8)
   (c9 ISA add-order low 8 high 9)
   
   ; soustaction (l'autoroute est limitée à 3 voies)
   ; tourner

   (r1 ISA rem-order over 0 under -1)
   (r2 ISA rem-order over 1 under 0)


   ;; TEST
   (first-goal ISA add arg1 5 arg2 2)

   (detect isa chunk)
 )

 
;; ---------- Add productions here ----------


;;etablir un premier goal focus pour le démarrage second-goal

;;;;;;;;;;;; Turns ;;;;;;;;;;;; 
;;(p turnL
;;   =goal>
;;      ISA                   turn
;;      xRelativePosition    -1
;;      xRelativePosition    =deviation
;;   =retrieval>
;;      ISA                   my_car
;;      positionX             =x_pos_car
;;==>
;;   =goal>
;;      ISA                   position
;;      id                    =0
;;   =retrieval>
;;      ISA                   rem-order
;;      first                 =x_pos_car 
;;)
;;
;;(p turnedLeft
;;   =goal>
;;      ISA                  position
;;      id                   0
;;      positionX            =posX
;;      positionY            =posY
;;   =retrieval>
;;      ISA                  rem-order
;;      first                posX
;;      second               =finalPosX
;;
;;==>
;;   =goal>
;;      ISA                  car
;;      id                   =0
;;      positionX            =finalPosX
;;   +imaginal>     ;; Réécriture du chunk position avec id=0
;;      ISA                 position
;;      id                  =0
;;      positionX           =finalPosX
;;      positionY           =posY
;;)
;;
;;(p turnR
;;   =goal>
;;      ISA                  turn
;;      xRelativePosition    1
;;      xRelativePosition    =deviation
;;   =retrieval>
;;      ISA                   my_car
;;      positionX             =x_pos_car
;;==>
;;   =goal>
;;      ISA                   position
;;      id                    =0
;;   =retrieval>
;;      ISA                   rem-order
;;      first                 =x_pos_car 
;;)
;;
;;(p turnedRight
;;   =goal>
;;      ISA                  position
;;      id                   0
;;      positionX            =posX
;;      positionY            =posY
;;   =retrieval>
;;      ISA                  rem-order
;;      first                posX
;;      second               =finalPosX
;;
;;==>
;;   =goal>
;;      ISA                  car
;;      id                   =0
;;      positionX            =finalPosX
;;   +imaginal>     ;; On attendant de savoir comment réécrire par dessus le chunk position avec id=0
;;      ISA                 position
;;      id                  =0
;;      positionX           =finalPosX
;;      positionY           =posY
;;)
;;
;;;;;;;;;;;;;; Brakes ;;;;;;;;;;;;
;;
;;;; A changer 
;;
;;(p brakeSoft
;;   =goal>
;;    ISA                   brake
;;    power                 1
;;    power                 =puissance_de_freinage
;;  =retrieval>
;;    ISA                   speed
;;    id                    0
;;    vitesse               =s
;;==>
;;  =goal>
;;   ;; ???
;;  =retrieval>
;;    ISA speed
;;    id                     0
;;    vitesse                =s-puissance_de_freinage
;;)
;;
;;;; A changer 
;;
;;(p brakeHard
;;   =goal>
;;    ISA       brake
;;    power     3
;;    power     =puissance_de_freinage
;;  =retrieval>
;;    ISA       speed
;;    id        0
;;    vitesse   =s
;;==>
;;  =goal>
;;   ; ???
;;  =retrieval>
;;    ISA speed
;;    id        0
;;    vitesse   =s-puissance_de_freinage
;;)
;;
;;;;;;;;;;;;;; Take info ;;;;;;;;;;;;
;;
;;;;(p lookLeft
;;;;   =goal>
;;;;    ISA                 a
;;;;    a                   a
;;;;  =retrieval>
;;;;    ISA                 a
;;;;    a                   a
;;;;    a                   a
;;;;==>
;;;;  =goal>
;;;;   ; ???
;;;;  =retrieval>
;;;;    ISA speed
;;;;    a         a
;;;;    a         a
;;;;)
;;
;;
;;;;;;;;;;;;;; Addition ;;;;;;;;;;;;
;;
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
      ISA         add-order
      low         =num1
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
      low         =count
      high        =newcount
  ==>
   =goal>
      ISA         add
      count       =newcount
   +retrieval>
      ISA         add-order
      low         =sum
)

(P increment-sum
   =goal>
      ISA         add
      sum         =sum
      count       =count
    - arg2        =count
   =retrieval>
      ISA         add-order
      low       =sum
      high      =newsum
  ==>
   =goal>
      ISA         add
      sum         =newsum
   +retrieval>
      ISA        add-order
      low        =count
)
(goal-focus first-goal)
)


(clear-all)

;; Classe voiture
(defclass voiture()
   (poids
   vitesse
   positionX
   positionY
   )
)

(defun show-learning (n &optional (graph t))
   (let (points)
   (dotimes (i n);; ici pour des blocs de 100
      (push (run-blocks 1 100) points)
   )
   ) 

   ;;(setf points (rev points))
   ;;(when graph
   ;;   (draw-graph points)
   ;;)
)

;;(defun rev(l)
;;   (cond
;;      ((null l) '())
;;      (T (append (rev (cdr l)) (list (car l))))
;;   )
;;) 


(defun run-blocks (blocks block-size)     
   (dotimes (i blocks)
      (setf retour (place_elements block-size))
   )
   retour
)

;; Fonction pour placer les voitures sur les voies
(defun place_elements (n-times &optional) ; TODO : (draw-highway nil))
   (format t "ca marcheee")
   (setf nbWin 0)
   (dotimes (i n-times)
      (setf tour 1)
      (setf not-win t) ; t = true, nil = false
      (setf res nil)
      (setf state nil)

      (setf *voitures* (create-voitures)); Creation de notre voiture et de la voiture accident
      ;; TODO : creation d'autres usagers = (setf *usagers* (create-usagers)); Creation des voitures des autres usagers si complexification


      (while not-win ; appeler le modèle tant qu'il n'a pas win
         ;; un genre de reset pour le modele je crois
         ;; 1er élément de la liste je crois, donc notre modele
         (setf (slot-value (car *voitures*) 'positionX) 0) 
         (setf (slot-value (car *voitures*) 'positionY) 0) 
         ;; 2nd élément de la liste je cris, donc la voiture accident
         (setf (slot-value (cadr *voitures*) 'positionX) 0)
         (setf (slot-value (cadr *voitures*) 'positionY) 10)

         (let ((choix-model (show-model-highway *voitures* res state))); Montre notre voiture et l'accident au modèle et enregistre la key pressée par le model
            ;; TODO
            ;; Quelles sont les strings a return pour choix-model ? freiner fort, freiner faible, tourner droite ou tourner gauche ?

            ;; 1 = frein faible, 2 = frein fort, 3 = turnR, 4 = turnL
            (when (string-equal "1" choix-model) (progn
               (setf tour (+ tour 1)) ;; incrémente le nombre de tour
               (setf (slot-value (car *voitures*) 'vitesse) (- (slot-value (car *voitures*) 'vitesse) 1)) ;; "moyen ? faible ?")  int ou string ?
               (setf state "Un state à choisir"))
            ) ; TODO

            (when (string-equal "2" choix-model) (progn
               (setf tour (+ tour 1)) ;; incrémente le nombre de tour
               (setf (slot-value (car *voitures*) 'vitesse) (- (slot-value (car *voitures*) 'vitesse) 2)) ;; "moyen ? faible ?")  int ou string ?
               (setf state "Un state à choisir"))
            ) ; TODO

            (when (string-equal "3" choix-model) (progn
               (setf tour (+ tour 1)) ;; incrémente le nombre de tour?
               (setf (slot-value (car *voitures*) 'positionX) (+ (slot-value (car *voitures*) 'positionX) 1))
               (setf state "Un state à choisir"))
            ) ; TODO

            (when (string-equal "4" choix-model) (progn
               (setf tour (+ tour 1)) ;; incrémente le nombre de tour
               (setf (slot-value (car *voitures*) 'positionX) (- (slot-value (car *voitures*) 'positionX) 1))
               (setf state "Un state à choisir"))
            ) ; TODO

            ;; check si la vitesse ne passe pas en dessous de un
            (if (< (slot-value (car *voitures*) 'vitesse)  1)
               (setf (slot-value (car *voitures*) 'vitesse) 1)
            )  
               
            (setf res "on verra") ;; changer les valeurs dans les if 

            ;; Les deux voitures sont sur la même case
            (if (= (slot-value (car *voitures*) 'positionX)  (slot-value (cadr *voitures*) 'positionX))  
               (if (= (slot-value (car *voitures*) 'positionY)  (slot-value (cadr *voitures*) 'positionY))  
                  (setf res "crash") 

                  ;; TODO : code a priori qu'on doit faire
                  ;(setf state " - un state a definir - ")
                  ;(setf not-win nil)
                  ;(show-model-result res state)

                  ;; pourquoi ce unless 0 ? eut être à enlever
                  (progn (setf not-win nil)
                     (unless (string-equal choix-model "0")(progn 
                        (setf state "final")
                        (show-model-result res state))
                     )
                  )
               )
            )

            ;; La voiture a dépasse l'accident, on gagne
            (if (> (slot-value (car *voitures*) 'positionX)  (slot-value (cadr *voitures*) 'positionX))    
               (setf res "win") ;; Les deux voitures sont sur la même case

               ;; TODO : code a priori qu'on doit faire
               ;(setf state " - un state a definir - ")
               ;(setf not-win nil)
               ;(show-model-result res state)

               ;; pourquoi ce unless 0 ? eut être à enlever
               (progn (setf not-win nil)
                  (unless (string-equal choix-model "0")(progn 
                     (setf state "final")
                     (show-model-result res state))
                  )
               )
            )

            ;;(loop for usager in *usagers* ; on traite toutes les voitures autour
            ;;   ; TODO : faire des if crash sur chaque voiture
            ;;)
    

            (when draw-highway
               ;;(print-model (car *voitures*))
               ;;(print-accident (cadr *voitures*))
               ;;(print-route)
            )

            (if (= res "win")
               (setf nbWin (+ nbWin 1))
            )
         )
      )
   )
   (format t "On a win ~d fois sur ~d essais" nbWin n-times)
)


(defun create-voitures ()
   ;; Création de l'instance des voitures
   (defparameter *model* (make-instance 'voiture))
   (defparameter *accident* (make-instance 'voiture))

   (setf (slot-value model 'poids) 1) ; poids pas aléatoire pour l'instant 
   (setf (slot-value model 'vitesse) 5)
   (setf (slot-value model 'positionX) 0) ; voie du milieu
   (setf (slot-value model 'positionY) 0) ; tout en bas de la route
   
   (setf (slot-value accident 'poids) 1) ; poids pas aléatoire pour l'instant 
   (setf (slot-value accident 'vitesse) 5)
   (setf (slot-value accident 'positionX) 0) ; voie du milieu
   (setf (slot-value accident 'positionY) 10) ; tout en haut de la route
   
   (defvar voitures-list (list *model* *accident*)) ; ajout des voitures dans une liste

   voitures-list
); return la liste avec [0] notre model et [1] l'accident

(defun create-usagers() ;; TODO :pour plus tard
   (defparameter *usager1* (make-instance 'voiture))
   (defparameter *usager2* (make-instance 'voiture))
   (defparameter *usager3* (make-instance 'voiture))

   ; boucle qui génère les voitures autours (innutile au début)
   ;;(loop for voiture in voitures-autour-list 
   ;;   do (progn
   ;;         (setf (slot-value voiture 'poids) (1) ; poids pas aléatoire pour l'instant 
   ;;         (setf (slot-value voiture 'vitesse) ("moyen")
   ;;         (setf (slot-value voiture 'positionX) (act-r-random 2) ; voie du milieu
   ;;         (setf (slot-value voiture 'positionY) (3+ (act-r-random 5))) ; quelque part sur l'axe y
   ;;         ;; Dimension selon la catégorie
   ;;         (case (slot-value voiture 'positionX)
   ;;            (1 (progn (setf (slot-value voiture 'positionX)-1)))
   ;;            (3 (progn (setf (slot-value voiture 'positionX) 1))))
   (defvar voitures-autour-list (list *usager1* *usager2*  *usager3*))
   voitures-autour-list
)

(defun show-model-highway (voitures &optional (res state))
   ;;(if (buffer-read 'goal) ;; s'il y a un chunk dans le buffers goal
   ;;
   ;;   ; Pas compris comment on choisi dans quel chunk mettre ça
   ;;
   ;;   ;; notre modele (le 'car' veut dire chopper la 1er element de la liste et pas la voiture en traduction anglaise !)
   ;;   (mod-focus-fct `(id ,0  weight ,(slot-value (car voitures) 'poids) state ,"non accidente"))  ; chunk type car
   ;;   (mod-focus-fct `(id ,0  positionX ,(slot-value (car voitures) 'positionX) positionY ,(slot-value (car voitures) 'positionY)))  ; chunk type position
   ;;   (mod-focus-fct `(id ,0  vitesse ,(slot-value (car voitures) 'vitesse)))  ; chunk type vitesse
   ;;
   ;;   ;; la voiture accident (le 'cadr' veut dire chopper le 2nd element de la liste)
   ;;   (mod-focus-fct `(id ,1  weight ,(slot-value (cadr voitures) 'poids) state ,"non accidente"))  ; chunk type car
   ;;   (mod-focus-fct `(id ,1  positionX ,(slot-value (cadr voitures) 'positionX) positionY ,(slot-value (car voitures) 'positionY)))  ; chunk type position
   ;;   (mod-focus-fct `(id ,1  vitesse ,(slot-value (cadr voitures) 'vitesse)))  ; chunk type vitesse
   ;;
   ;;   ; TODO
   ;;   (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
   ;;                          `((isa /* mettre nom de la procédure à goal */ 
   ;;                              var1, value1
   ;;                              var2, value2
   ;;                              ...
   ;;                              )))))
   ;;)
   ;;(run-full-time 10)
   ;;*model-action*
)

(defun show-model-result (res state)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
      (mod-focus-fct `(result ,res
                        state ,state)
      )
      (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `(isa check_state
                                 result ,res
                                 state ,state)
                           )
                        )
      )
   )
   (run-full-time 10)
)

(defun draw-graph (points)
  (let ((w (open-exp-window "Data" :width 550 :height 460 :visible t)))
      (allow-event-manager w)
      (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window "Data")

      (dotimes (i 11)
         (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (* (- 1 (* i .1)) 3)) :window "Data")
         (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window "Data")
      )
    
      (let ((x 50))
         (mapcar (lambda (a b) (add-line-to-exp-window  (list x (floor (- 410 (* a 400))))
                                                         (list (incf x 25) (floor (- 410 (* b 400))))
                                                         :color 'blue :window "Data")
                  )
            (butlast points) (cdr points)
         )
      )
      (allow-event-manager w)
   )
)

(define-model conductor
    
   ;;(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)
   (sgp :esc t :lf .05)
   (install-device (open-exp-window "" :visible nil))

   ;; ---------- Add Chunk-types here ----------
   (chunk-type check_state result state)
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
   ;; exemple d'une voiture
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
   ;;    +manual>
   ;;      cmd press-key
   ;;      key "4"
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
   ;;    +manual>
   ;;      cmd press-key
   ;;      key "4"
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
   ;;    +manual>
   ;;      cmd press-key
   ;;      key "3"
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
   ;;    +manual>
   ;;      cmd press-key
   ;;      key "3"
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
   ;;    =goal>
   ;;       ;; ???
   ;;     =retrieval>
   ;;       ISA speed
   ;;       id                     0
   ;;       vitesse                =s-puissance_de_freinage
   ;;    +manual>
   ;;       cmd press-key
   ;;       key "1"
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
   ;;    =goal>
   ;;       ; ???
   ;;    =retrieval>
   ;;       ISA speed
   ;;       id        0
   ;;       vitesse   =s-puissance_de_freinage
   ;;    +manual>
   ;;       cmd press-key
   ;;       key "2"
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


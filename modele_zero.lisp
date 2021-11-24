(clear-all)

;; Classe voiture
(defclass voiture()
   (poids
   vitesse
   positionX
   positionY
   )
)

(defun run-blocks (blocks block-size)     
   (dotimes (i blocks)
      (setf retour (place_elements block-size)))
      retour)

;; Fonction pour placer les voitures sur les voies
(defun place_elements(n-times &optional (draw-valises nil))

   (setf moyenne 0)
   (dotimes (i n-times)
      (setf compteur 1)
      (setf not-win t)
      (setf res nil)
      (setf state nil)

      (setf *voitures* (create-voiture)); Creation des voitures


      (while not-win ; appeler le modèle tant qu'il n'a pas win
         ;; un genre de reset pour le modele je crois
         ;; 1er élément de la liste je crois, donc notre modele
         (setf (slot-value (car *voitures*) 'positionX) 0) 
         (setf (slot-value (car *voitures*) 'positionY) 0) 
         ;; 2nd élément de la liste je cris, donc la voiture accident
         (setf (slot-value (cadr *voitures*) 'positionX) 0)
         (setf (slot-value (cadr *voitures*) 'positionY) 10)

         (let ((choix-model (show-model-highway *voitures* res state))); Montre notre voiture et l'accident au modèle et enregistre la key pressée par le model
            
            (when (string-equal "1" choix-model) (progn
               (setf compteur (+ compteur 1)) ;; incrémente compteur
               (setf (slot-value (caddr *valises*) 'couche) 2)
               (setf state "weight-problem"))) ; Met la troisième valise en couche 2
                ;; mettre state à weight-problem si key 1
            (when (string-equal "2" choix-model) (progn
               (setf compteur (+ compteur 1))
               (setf (slot-value (cadr *valises*) 'couche) 2)
               (setf state "weight-problem-2"))) ; Met la deuxième valise en couche 2
            (setf res "win") ;; De base on win
            (setf poids-tot-couche-1 0)
            (setf poids-tot-couche-2 0)
            (loop for valise in *valises* ; boucle sur les valises choisi par le modèle
               do (if (= (slot-value valise 'couche) 1)
                     ;; Si la valise est couche 1
                     (setf poids-tot-couche-1 (+ poids-tot-couche-1 (slot-value valise 'poids))) ; Adition du poids de la valise
                     ;; Si la valise est couche 2
                     (setf poids-tot-couche-2 (+ poids-tot-couche-2 (slot-value valise 'poids))))) ; Adition du poids de la valise
            ;(setf choix-model "0")
            (if (> poids-tot-couche-2 poids-tot-couche-1)
               (setf res "lose") ; Si les valises en couche 2 sont plus lourdes -> lose
               (progn (setf not-win nil)
                      (unless (string-equal choix-model "0")(progn 
                        (setf state "final")
                        (show-model-result res state))))))
            (when draw-valises
            (print-valise (car *valises*))
            (print-valise (cadr *valises*))
            (print-valise (caddr *valises*))
               (if (and (= (slot-value (car *valises*) 'couche) 1) (= (slot-value (cadr *valises*) 'couche) 1) (= (slot-value (caddr *valises*) 'couche) 1))
                  (progn
                     (setf nb 0)
                     (setf grandevalise (car *valises*))
                     (loop for valise in *valises*
                        do (if (= (slot-value valise 'categorie) 1)
                           (setf nb (+ nb 1))
                           (setf grandevalise valise)))
                     (if (>= nb 2)
                        (progn 
                           (draw2little)
                           (draw-valise grandevalise)
                        )
                        (progn
                           (format t "Niveau 1:~%")
                           (loop for valise in *valises*
                              do (when (= (slot-value valise 'couche) 1)
                                 (progn (draw-valise valise) (format t "~%"))))
                           (format t "~%Niveau 2:~%")
                           (loop for valise in *valises*
                              do (when (= (slot-value valise 'couche) 2)
                                 (draw-valise valise)))
                        ))
                  )
                  (progn 
                     (format t "Niveau 1:~%")
                     (loop for valise in *valises*
                        do (when (= (slot-value valise 'couche) 1)
                           (progn (draw-valise valise) (format t "~%"))))
                     (format t "~%Niveau 2:~%")
                     (loop for valise in *valises*
                        do (when (= (slot-value valise 'couche) 2)
                           (draw-valise valise)))))))



(defun create-voitures()
   ;; Création de l'instance des voitures
   (defparameter *model* (make-instance 'voiture))
   (defparameter *accident* (make-instance 'voiture))
   ;;(defparameter *voiture* (make-instance 'voiture)) ;; pour plus tard

   (setf (slot-value model 'poids) (1) ; poids pas aléatoire pour l'instant 
   (setf (slot-value model 'vitesse) ("rapide")
   (setf (slot-value model 'positionX) (0) ; voie du milieu
   (setf (slot-value model 'positionY) (0) ; tout en bas de la route
   
   (setf (slot-value accident 'poids) (1) ; poids pas aléatoire pour l'instant 
   (setf (slot-value accident 'vitesse) ("lent")
   (setf (slot-value accident 'positionX) (0) ; voie du milieu
   (setf (slot-value accident 'positionY) (10) ; tout en haut de la route

   
   (defvar voitures-list(list *model* *accident*)) ; ajout des voitures dans une liste
   ;;(defvar voitures-autour-list(list *voiture* *voiture*)) ; si jamais pour plus tard

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

   voitures-list); return la liste avec [0] notre model et [1] l'accident
   

(defun show-model-highway(voitures &optional res state)
   (if (buffer-read 'goal) ;; s'il y a un chunk dans le buffers goal

      ; Pas compris comment on choisi dans quel chunk mettre ça

      ;; notre modele (le 'car' veut dire chopper la 1er element de la liste et pas la voiture en traduction anglaise !)
      (mod-focus-fct `(id ,0  weight ,(slot-value (car voitures) 'poids) state ,"non accidente"))  ; chunk type car
      (mod-focus-fct `(id ,0  positionX ,(slot-value (car voitures) 'positionX) positionY ,(slot-value (car voitures) 'positionY)))  ; chunk type position
      (mod-focus-fct `(id ,0  vitesse ,(slot-value (car voitures) 'vitesse)))  ; chunk type vitesse

      ;; la voiture accident (le 'cadr' veut dire chopper le 2nd element de la liste)
      (mod-focus-fct `(id ,1  weight ,(slot-value (cadr voitures) 'poids) state ,"non accidente"))  ; chunk type car
      (mod-focus-fct `(id ,1  positionX ,(slot-value (cadr voitures) 'positionX) positionY ,(slot-value (car voitures) 'positionY)))  ; chunk type position
      (mod-focus-fct `(id ,1  vitesse ,(slot-value (cadr voitures) 'vitesse)))  ; chunk type vitesse

      (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `((isa arrange-state c1 ,(slot-value (car valises) 'categorie)  c2 ,(slot-value (cadr valises) 'categorie) c3 ,(slot-value (caddr valises) 'categorie)
                                 p1 ,(slot-value (car valises) 'poids)  p2 ,(slot-value (cadr valises) 'poids) p3 ,(slot-value (caddr valises) 'poids)
                                 result , res
                                 state , state
                                 first-c , nil
                                 second-c, nil))))))






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


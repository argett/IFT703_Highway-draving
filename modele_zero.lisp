(clear-all)

(defvar *model-action* ) ; La variable que le model devra remplir (liste de valise)
(defvar voitures-list nil)

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
(defun place_elements (n-times &optional (draw-highway nil))   
   (let (scores (need-to-remove (add-key-monitor)))

      (setf simulation 0)
      (dotimes (i n-times)
         (setf simulation (+ simulation 1))
         (setf *voitures* (create-voitures)); Creation de notre voiture et de la voiture accident
         (setf m_oldSped (slot-value (car *voitures*) 'vitesse)) 
         (setf m_oldPosX (slot-value (car *voitures*) 'positionX)) 
         (setf m_oldPosY (slot-value (car *voitures*) 'positionY)) 

         (setf nbTry 0)
         (setf not-win t) ; t = true, nil = false
         (setf res nil)
         (setf state nil)

         ;; TODO : creation d'autres usagers = (setf *usagers* (create-usagers)); Creation des voitures des autres usagers si complexification

         (while not-win ; appeler le modèle tant qu'il n'a pas win ou pas crash         
            (if (> nbTry 1) 
               (progn
                  (setf res nil)
               )
            )

            ;; On reset la position des modele dans la cas où on a perdu avant
            ; 1er élément de la liste, donc notre modele
            (setf (slot-value (car *voitures*) 'vitesse) m_oldSped) 
            (setf (slot-value (car *voitures*) 'positionX) m_oldPosX) 
            (setf (slot-value (car *voitures*) 'positionY) m_oldPosY) 

            (let ((choix-model (show-model-highway *voitures* res state))); Montre notre voiture et l'accident au modèle et enregistre la key pressée par le model
               (setf nbTry (+ nbTry 1))

               ;; 1 = frein faible, 2 = frein fort, 3 = tourne a droite, 4 = tourne a gauche, 0 = ne rien faire
               (if (string-equal "1" choix-model) 
                  (progn
                     (setf (slot-value (car *voitures*) 'vitesse) (- (slot-value (car *voitures*) 'vitesse) 1))
                     (setf state "1")
                     (format t "Le modele freine doucement v= ~s ~C~C" (slot-value (car *voitures*) 'vitesse) #\return #\linefeed )
                  )
               )

               (if (string-equal "2" choix-model) 
                  (progn
                     (setf (slot-value (car *voitures*) 'vitesse) (- (slot-value (car *voitures*) 'vitesse) 2))
                     (setf state "2")
                     (format t "Le modele freine fort v= ~s ~C~C" (slot-value (car *voitures*) 'vitesse) #\return #\linefeed )
                  )
               )

               (if (string-equal "3" choix-model) 
                  (progn
                     (setf (slot-value (car *voitures*) 'positionX) (+ (slot-value (car *voitures*) 'positionX) 1))
                     (setf state "3")
                     (format t "Le modele tourne a droite ~C~C" #\return #\linefeed )
                  )
               )

               (if (string-equal "4" choix-model) 
                  (progn
                     (setf (slot-value (car *voitures*) 'positionX) (- (slot-value (car *voitures*) 'positionX) 1))
                     (setf state "4")
                     (format t "Le modele tourne a gauche ~C~C" #\return #\linefeed )
                  )
               )
               (if (string-equal "5" choix-model) 
                  (progn
                     ; ne change rien
                     (setf state "5")
                     (format t "Le modele ne fait rien ~C~C" #\return #\linefeed )
                  )
               )

               ; pas besoin de check si un seul tour
               ;;; check si la vitesse ne passe pas en dessous de 1
               ;(if (< (slot-value (car *voitures*) 'vitesse)  1)
               ;   (setf (slot-value (car *voitures*) 'vitesse) 1)
               ;)  

               ;
               ;;(if (> (slot-value (car *voitures*) 'position)  1)
               ;;   (progn
               ;;      (setf (slot-value (car *voitures*) 'position) 1)
               ;;      (setf res "crash") 
               ;;      (setf not-win t)
               ;;   )
               ;;) 
               ;;(if (< (slot-value (car *voitures*) 'position)  -1)
               ;;   (progn
               ;;      (setf (slot-value (car *voitures*) 'position) -1)
               ;;      (setf res "crash") 
               ;;      (setf not-win t)
               ;;   )
               ;;) 

               ;; on fait avancer la voiture selon sa vitesse
               (setf (slot-value (car *voitures*) 'positionY) (+ (+ (slot-value (car *voitures*) 'positionY) (slot-value (car *voitures*) 'vitesse)) (slot-value (car *voitures*) 'poids)))

               ;; L'accident et le modele sont sur la même case ou si le modele freine alors que l'usager se trouve juste derrière
               ;; ou si le modele enfonce le terre-plein central ou sort de la route
               (if (or  (or   (and (= (slot-value (car *voitures*) 'positionX)  (slot-value (cadr *voitures*) 'positionX))
                                   (>= (slot-value (car *voitures*) 'positionY)  (slot-value (cadr *voitures*) 'positionY)))
                              (and (= (slot-value (car *voitures*) 'positionX)  (slot-value (caddr *voitures*) 'positionX))
                                   (or  (string-equal choix-model "2")
                                        (string-equal choix-model "1"))))
                        (or   (or  (> (slot-value (car *voitures*) 'positionX)  1)
                                   (< (slot-value (car *voitures*) 'positionX)  -1))))        
                  (progn
                     (setf res "crash")
                     (setf not-win t)
                     (format t "Le modele a crash ~C~C" #\return #\linefeed )
                  )
                  (progn
                     (setf res "esquive")
                     (setf not-win nil)
                     (format t "Le modele a esquive ~C~C" #\return #\linefeed )
                  )
               )

               ;; si 1 tour, il faut juste qu'elle ne crash pas
               ;;;; La voiture a depasse l'accident, on gagne
               ;;(if (> (slot-value (car *voitures*) 'positionX)  (slot-value (cadr *voitures*) 'positionX))    
               ;;   (setf res "win") ;; Les deux voitures sont sur la même case
               ;;
               ;;   ;; TODO : code a priori qu'on doit faire
               ;;   ;(setf state " - un state a definir - ")
               ;;   ;(setf not-win nil)
               ;;   ;(show-model-result res state)
               ;;
               ;;   ;; pourquoi ce unless 0 ? eut être à enlever
               ;;   (progn (setf not-win nil)
               ;;      (unless (string-equal choix-model "0")(progn 
               ;;         (setf state "final")
               ;;         (show-model-result res state))
               ;;      )
               ;;   )
               ;;)

               ;;(loop for usager in *usagers* ; on traite toutes les voitures autour
               ;;   ; TODO : faire des if crash sur chaque voiture
               ;;)
      
               (show-model-result res state)

               ;;(when draw-highway
               ;;   (format t "TODO : print l'autoroute")
               ;;   (print-model (car *voitures*))
               ;;   (print-accident (cadr *voitures*))
               ;;   (print-route)
               ;;)
            )
         )
         (format t "INFO : ~d /??? tentatives ~C~C" nbTry #\return #\linefeed )
         (format t "--------------- ~d / ~d ---------------~C~C" simulation n-times #\return #\linefeed )
      )
      (format t "~C~C ~C~C ~C~C" #\return #\linefeed #\return #\linefeed #\return #\linefeed )
   )
)


(defun create-voitures ()
   ;; Création de l'instance des voitures
   (defparameter *model* (make-instance 'voiture))
   (defparameter *accident* (make-instance 'voiture))
   (defparameter *usager* (make-instance 'voiture))
   

   (setf (slot-value *model* 'poids) (act-r-random 2)) 
   (setf (slot-value *model* 'vitesse) (+ (act-r-random 2) 3)) ; vitesse entre 3 et 4 
   (setf (slot-value *model* 'positionX) (- (act-r-random 3) 1)) ; voie entre -1 et 1
   (setf (slot-value *model* 'positionY) (act-r-random 2)) ; en bas de la route
   
   (setf (slot-value *accident* 'poids) 0)      ; pas d'importance
   (setf (slot-value *accident* 'vitesse) 0)    ; pas d'importance
   (setf (slot-value *accident* 'positionX) (- (act-r-random 3) 1))  ; voie entre -1 et 1
   (setf (slot-value *accident* 'positionY) 2)  ; en haut de la route
   

   ; le but de cette voiture derriere est d'empecher notre modele de freiner fort, sinon crash
   (setf (slot-value *usager* 'poids) 0) ; poids pas aléatoire pour l'instant 
   (setf (slot-value *usager* 'vitesse) 0)
   ; on rend 1/2 chance de mettre l'usager derriere pour augmenter les probas de crash, et 1/2 de la mettre a gauche
   (setf behind (act-r-random 2))
   (if (= behind 1)
      (setf (slot-value *usager* 'positionX) -1)
      (setf (slot-value *usager* 'positionX)  (slot-value *model* 'positionX))
   )
   (setf (slot-value *usager* 'positionY) (- (slot-value *model* 'positionY) 1)) ; juste derriere l'utilisateur


   (print-Highway *model* *accident* *usager*)

   (setf voitures-list (list *model* *accident* *usager*)) ; ajout des voitures dans une listere))

   voitures-list
); return la liste avec [0] notre model, [1] l'accident et [2] la BMW qui te colle derrière

;(defun create-usagers() ;; TODO :pour plus tard
;   (defparameter *usager1* (make-instance 'voiture))
;   (defparameter *usager2* (make-instance 'voiture))
;   (defparameter *usager3* (make-instance 'voiture))
;
;   ; boucle qui génère les voitures autours (innutile au début)
;   (loop for voiture in voitures-autour-list 
;      do (progn
;            (setf (slot-value voiture 'poids) 1) ; poids pas aléatoire pour l'instant 
;            (setf (slot-value voiture 'vitesse) (act-r-random 3)) 
;            (setf (slot-value voiture 'positionX) (act-r-random 2)) ; voie du milieu
;            (setf (slot-value voiture 'positionY) (+ 3 (act-r-random 5))) ; quelque part sur l'axe y
;            ;; Dimension selon la catégorie
;            (case (slot-value voiture 'positionX)
;               (1 (progn (setf (slot-value voiture 'positionX)-1)))
;               (3 (progn (setf (slot-value voiture 'positionX) 1)))
;            )
;            )
;   )
;   (defvar voitures-autour-list (list *usager1* *usager2*  *usager3*))
;   voitures-autour-list
;)

(defun show-model-highway (voitures &optional res state)
   (format t "Goal buffer = ~s ; res = ~s ; state = ~s ~C~C" (buffer-read 'goal) res state #\return #\linefeed )
   (if (buffer-read 'goal) ;; s'il y a un chunk dans le buffers goal 
      (mod-focus-fct `(m_positionX ,(slot-value (car voitures) 'positionX) 
                        m_positionY ,(slot-value (car voitures) 'positionY) 
                        m_vitesse   ,(slot-value (car voitures) 'vitesse) 
                        result      ,nil ;; pourquoi mettre res ici ?
                        state       ,"end_set" ;; on tente de se remember directement
                     ) 
      )
      (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `((isa check-state 
                                m_weight    ,(slot-value (car voitures) 'poids) 
                                 m_positionX ,(slot-value (car voitures) 'positionX) 
                                 m_positionY ,(slot-value (car voitures) 'positionY) 
                                 m_vitesse   ,(slot-value (car voitures) 'vitesse) 
                                 a_positionX ,(slot-value (cadr voitures) 'positionX) 
                                 a_positionY ,(slot-value (cadr voitures) 'positionY) 
                                 a_vitesse   ,(slot-value (cadr voitures) 'vitesse) 
                                 u_positionX ,(slot-value (caddr voitures) 'positionX) 
                                 result      ,nil
                                 state       start
                              ))
                           )
                        )
      )
   )
   (format t "ACTR Action ~C~C" #\return #\linefeed )
   (run-full-time 10)
   *model-action*
)


(defun show-model-result (res etat)
   (if (buffer-read 'goal) ; s'il y a un chunk dans le buffers goal
      (progn
         (mod-focus-fct `(result ,res
                           state ,"startEnregistre")
         )
      )
      (progn
         (goal-focus-fct (car (define-chunks-fct ; crée un nouveau chunk et le met dans le goal
                             `(isa check-state
                                 result ,res
                                 state ,"startEnregistre")
                           )
                        )
         )
      )
   )
   (run-full-time 10)
)


(defvar *key-monitor-installed* nil)
(defun add-key-monitor ()
   (unless *key-monitor-installed*
      (add-act-r-command "1hit-bj-key-press" 'respond-to-keypress 
                        "highway task key output monitor")
      (monitor-act-r-command "output-key" "1hit-bj-key-press")
      (setf *key-monitor-installed* t)
   )
)

(defun respond-to-keypress (model key)
   (if model
      (setf *model-action* key)
      (clear-exp-window)
   )
   key
)

(defun remove-key-monitor ()
  (remove-act-r-command-monitor "output-key" "1hit-bj-key-press")
  (remove-act-r-command "1hit-bj-key-press")
  (setf *key-monitor-installed* nil)
)

(defun print-highway (*model* *accident* *usager*)
   (setf nbY (+ (slot-value *accident* 'positionY) 1))
   (format t "~C~C ~C~C^   ^   ^   ^~C~C"  #\return #\linefeed #\return #\linefeed #\return #\linefeed )

   (while (not (= nbY -1))
      (format t "|")
      (if (= nbY (slot-value *model* 'positionY))
         (progn
            (if (= (slot-value *model* 'positionX) -1)
               (format t " M ")
               (format t "   ")
            )
            (format t "|")
            (if (= (slot-value *model* 'positionX) 0)
               (format t " M ")
               (format t "   ")
            )
            (format t "|")
            (if (= (slot-value *model* 'positionX) 1)
               (format t " M ")
               (format t "   ")
            )
            (format t "|    avec position Y = ~d ; vitesse = ~d ; poids = ~d ~C~C" (slot-value *model* 'positionY) (slot-value *model* 'vitesse) (+ (slot-value *model* 'poids) 1) #\return #\linefeed )
         )
      )
      
      (if (= nbY (slot-value *accident* 'positionY))
         (progn
            (if (= (slot-value *accident* 'positionX) -1)
               (format t " A ")
               (format t "   ")
            )
            (format t "|")
            (if (= (slot-value *accident* 'positionX) 0)
               (format t " A ")
               (format t "   ")
            )
            (format t "|")
            (if (= (slot-value *accident* 'positionX) 1)
               (format t " A ")
               (format t "   ")
            )
            (format t "|    avec position Y = ~d ~C~C" (slot-value *accident* 'positionY) #\return #\linefeed )
         )
      )

      (if (not (or (= nbY (slot-value *accident* 'positionY))
                     (= nbY (slot-value *model* 'positionY))))
         (format t "   |   |   | ~C~C" #\return #\linefeed )
      )
      (setf nbY (- nbY 1))
   )

   ; l'usager est toujours derriere
   (if (= (slot-value *usager* 'positionX) -1)
      (format t "| U ")
      (format t "|   ")
   )
   (format t "|")
   (if (= (slot-value *usager* 'positionX) 0)
      (format t " U ")
      (format t "   ")
   )
   (format t "|")
   (if (= (slot-value *usager* 'positionX) 1)
      (format t " U |")
      (format t "   |")
   )

   (format t "~C~C ~C~C"  #\return #\linefeed #\return #\linefeed )
)

;;(defun draw-graph (points)
;;  (let ((w (open-exp-window "Data" :width 550 :height 460 :visible t)))
;;      (allow-event-manager w)
;;      (add-line-to-exp-window '(50 0) '(50 420) :color 'white :window "Data")
;;
;;      (dotimes (i 11)
;;         (add-text-to-exp-window :x 5 :y (+ 5 (* i 40)) :width 35 :text (format nil "~3,1f" (* (- 1 (* i .1)) 3)) :window "Data")
;;         (add-line-to-exp-window (list 45 (+ 10 (* i 40))) (list 550 (+ 10 (* i 40))) :color 'white :window "Data")
;;      )
;;    
;;      (let ((x 50))
;;         (mapcar (lambda (a b) (add-line-to-exp-window  (list x (floor (- 410 (* a 400))))
;;                                                         (list (incf x 25) (floor (- 410 (* b 400))))
;;                                                         :color 'blue :window "Data")
;;                  )
;;            (butlast points) (cdr points)
;;         )
;;      )
;;      (allow-event-manager w)
;;   )
;;)


(define-model conductor
    
   (sgp :v t :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)
   ;;(sgp :esc t :lf .05)
   (install-device (open-exp-window "" :visible nil))

   ;; ------------------------------ Add Chunk-types here ------------------------------

   (chunk-type check-state state result m_weight m_positionX m_positionY m_vitesse a_positionX a_positionY a_vitesse u_positionX action )
   (chunk-type learned-info result m_weight m_positionX m_positionY m_vitesse a_positionX a_positionY a_vitesse u_positionX)
   (chunk-type car id weight)
   (chunk-type position id positionX positionY)
   (chunk-type speed id vitesse)

   ;(declare-buffer-usage goal check-state :all )

   ;; ------------------------------ Add Chunks here ------------------------------

   (define-chunks 
      ;; les differents states du goal
      (save_model_weight isa chunk) 
      (save_model_pos   isa chunk)
      (save_model_speed isa chunk)
      (save_acc_pos     isa chunk)
      (save_acc_speed   isa chunk)
      (save_usager_pos  isa chunk)
      (re-start-suite   isa chunk)

      (remembering      isa chunk) 
      (choice           isa chunk) 
      (choseAction      isa chunk)
      (applyAction      isa chunk) 

      (brake_soft       isa chunk) 
      (brake_hard       isa chunk) 
      (turn_right       isa chunk) 
      (turn_left        isa chunk)
      
      (enregistre       isa chunk)  
      (finish           isa chunk) 
      (finish2          isa chunk) 
   )

   (add-dm
      ;;Les voitures sont générées par le code LISP
      (brakeSoft isa brake power 1)
      (brakeHard isa brake power 2)

      (turnR isa turn xRelativePosition 1) 
      (turnL isa turn xRelativePosition -1)
   )

   
   ;; ------------------------------ Add productions here ------------------------------
        

   ;; -------------------- Première procédure  --------------------
   ;; Se lance au tout début afin d'initialiser les chunks
   ;; -------------------------------------------------------------

   (p start
      =goal>
         isa            check-state
         state          start
         m_weight       =a
         m_positionX    =b
         m_positionY    =c
         m_vitesse      =d
         a_positionX    =x
         a_positionY    =y
         a_vitesse      =z
         u_positionX    =u
      ==>
      =goal>
         isa            check-state
         state          save_model_weight
         m_weight       =a
         m_positionX    =b
         m_positionY    =c
         m_vitesse      =d
         a_positionX    =x
         a_positionY    =y
         a_vitesse      =z
         u_positionX    =u
   )
   
   ;; --------------- Start et enregistrement des donnees dans des chunks  ---------------
   ;; il faut faire plusieurs procedures car on a plusieurs chunks car apparemment 
   ;; cela essemble plus a un humain que de stocker ca en plusieurs fois
   ;; ------------------------------------------------------------------------------------

   (p set_model_0
      =goal>
         isa            check-state
         state          save_model_weight
         m_weight       =a
      ==>
      +imaginal>
         isa            car
         id             0
         weight         =a
      -imaginal> 
      =goal>
         state          save_model_pos
   )

   (p set_model_1
      =goal>
         isa            check-state
         state          save_model_pos
         m_positionX    =b
         m_positionY    =c
      ?imaginal>
         state          free
      ==>
      +imaginal> 
         isa            position
         id             0
         positionX      =b
         positionY      =c
      -imaginal> 
      =goal>
         state save_model_speed
   )

   (p set_model_2
      =goal>
         isa            check-state
         state          save_model_speed
         m_vitesse      =d
      ?imaginal>
         state          free
      ==>
      +imaginal> 
         isa            speed
         id             0
         vitesse        =d
      -imaginal> 
      =goal>
         state          save_acc_pos
   )

   (p set_accdt_1
      =goal>
         isa            check-state
         state          save_acc_pos
         a_positionX    =x
         a_positionY    =y
      ?imaginal>
         state          free
      ==>
      +imaginal> 
         isa            position
         id             1
         positionX      =x
         positionY      =y
      -imaginal> 
      =goal>
         state          save_acc_speed
   )

   (p set_accdt_2
      =goal>
         isa            check-state
         state          save_acc_speed
         m_vitesse      =z
      ?imaginal>
         state          free
      ==>
      +imaginal> 
         isa            speed
         id             1
         vitesse        =z
      -imaginal> 
      =goal>
         state          save_usager_pos
   )

   (p set_usager_1
      =goal>
         isa            check-state
         state          save_usager_pos
         positionX      =u
      ?imaginal>
         state          free
      ==>
      +imaginal> 
         isa            position
         id             2
         positionX      =u
         positionY      nil
      -imaginal> 
      =goal>
         state          "end_set"
   )

   ;; ----------------- Enregistrement des donnees  -----------------
   ;; On essaie de se souvenir si la situation s'est déjà présenté
   ;; Si on se souviens d'une situation win : 
   ;;    on applique la meme chose
   ;; Si on se souviens d'une situation loose :
   ;;    on a lose en freinant soft -> on freine hard
   ;;    on a lose en freinant hard -> on turn right
   ;;    on a lose en turn right -> on turn left
   ;;
   ;; Si on ne se souvient pas, on freine soft
   ;; ---------------------------------------------------------------

   (p try_load
      =goal>
         isa            check-state
         state          "end_set"
         m_weight       =a
         m_positionX    =b
         m_positionY    =c
         m_vitesse      =d
         a_positionX    =x
         a_positionY    =y
         a_vitesse      =z
         u_positionX    =u
      ?imaginal>
         state          free
      ==>
      -imaginal> ;; Pour sauvegarder l'imaginal de set_accdt_2
      +retrieval> 
         isa            learned-info
         m_weight       =a
         m_positionX    =b
         m_positionY    =c
         m_vitesse      =d
         a_positionX    =x
         a_positionY    =y
         a_vitesse      =z
         u_positionX    =u
      =goal>
         state          remembering
   )

   (p remember-organization
      =goal>
         isa            check-state
         state          remembering
      =retrieval>
         isa            learned-info
         result         =res
         action         =act
      ==>
      =goal>
         isa            learned-info
         state          choseAction
         result         =res
         action         =act
   )

   (p remember-win
      =goal>
         isa            learned-info
         state          choseAction
         result        "esquive"
         action         =act
      ==>
      =goal>
         state          applyAction
         action         =act
   )


;;---------------------------------------------


   (p remember-lose-b-soft
      =goal>
         state          choseAction
         result        "crash"
         action         "1"
      ==>
      =goal>
         state          applyAction
         action         "2"
   )

   (p remember-lose-b-hard
      =goal>
         state          choseAction
         result        "crash"
         action         "2"
      ==>
      =goal>
         state          applyAction
         action         "3"
   )

   (p remember-lose-t-rigth
      =goal>
         state          choseAction
         result        "crash"
         action         "3"
      ==>
      =goal>
         state          applyAction
         action         "4"
   )

   (p remember-lose-t-left
      =goal>
         state          choseAction
         result        "crash"
         action         "4"
      ==>
      =goal>
         state          applyAction
         action         "5"
   )


   (p doesnt-remember-organization
      =goal>
         state          remembering
      ?retrieval>
         buffer         failure
      ==>
      =goal>
         state          applyAction
         action         "1"
   )

   ;;;;;;;;;;;; Brakes ;;;;;;;;;;;;
   
   (p brakeSoft
      =goal>  
         state          applyAction
         action         "1"
     ?manual>
         state          free
   ==>
      =goal>
         state          nil
         action         "1"
      +manual>
         cmd            press-key
         key            "1"
   )
   
   (p brakeHard
      =goal>
         state          applyAction
         action         "2"
     ?manual>
         state          free
   ==>
      =goal>
         state          nil
         action         "2"
      +manual>
         cmd            press-key
         key            "2"
   )

   ;;;;;;;;;;;; Turns ;;;;;;;;;;;; 
   
   (p turnR
      =goal>
         state          applyAction
         action         "3"
     ?manual>
         state          free
   ==>
      =goal>
         state          nil
         action         "3"
       +manual>
         cmd            press-key
         key            "3"
   )

   (p turnL
      =goal>
         state          applyAction
         action         "4"
     ?manual>
         state          free
   ==>
      =goal>
         state          nil
         action         "4"
       +manual>
         cmd            press-key
         key            "4"
   )

   
   ;;;;;;;;;;;; do nothing is the better sometimes ;;;;;;;;;;;; 

   (p do-nothing
      =goal>
         state          applyAction
         action         "5"
     ?manual>
         state          free
   ==>
      =goal>
         state          nil
         action         "5"
       +manual>
         cmd            press-key
         key            "5"
   )
   
   ;;;;;;;;;;;;;;;; Take info ;;;;;;;;;;;;
   ;;
   ;;(p lookLeft
   ;;   =goal>
   ;;    ISA            a
   ;;    a              a
   ;;  =retrieval>
   ;;    ISA            a
   ;;    a              a
   ;;    a              a
   ;;==>
   ;;  =goal>
   ;;   ; ???
   ;;  =retrieval>
   ;;    ISA speed
   ;;    a              a
   ;;    a              a
   ;;)
   ;;
   ;(goal-focus check-state)

   
   ;;;;;;;;;;;;;;;;;;;; Sauvegardes ;;;;;;;;;;;;;;;;;;;;


   (p start-save
      =goal>
         state          "startEnregistre"
         result         =res
         action         =act
   ==>
      =goal>
         state          enregistre
         result         =res
         action         =act
   )

   (p save
      =goal>
         state          enregistre
         result         =res
         action         =act
         m_weight       =a
         m_positionX    =b
         m_positionY    =c
         m_vitesse      =l
         a_positionX    =d
         a_positionY    =e
         a_vitesse      =f
         u_positionX    =u
      ?imaginal>
         state          free    
      ==>
      +imaginal>
         isa            learned-info
         result         =res
         action         =act
         m_weight       =a
         m_positionX    =b
         m_positionY    =c
         m_vitesse      =l
         a_positionX    =d
         a_positionY    =e
         a_vitesse      =f
         u_positionX    =u
      -imaginal>
      =goal>
         state          finish
   )

   ;;;;;;;;;;;;;;; On delete le buffer imaginal ;;;;;;;;;;;;;;;

   (p finish_saving
      =goal>
         state          finish
         result         =res
      ==>
      -imaginal>
      =goal>
         state          finish2
         result         =res
   )
   
   (p clear-goal
      =goal>
         state          finish2
         result         "esquive"
      ==>
      -goal>
   )
)
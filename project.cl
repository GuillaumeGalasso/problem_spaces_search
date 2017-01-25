; action 1 : remplir R1
; action 2 : remplir R2
; action 3 : vider R1
; action 4 : vider R2
; action 5 : transvaser la toltalité de R2 dans R1
; action 6 : transvaser la toltalité de R1 dans R2
; action 7 : transvaser une partie de R2 dans R1 (R1 est rempli)
; action 8 : transvaser une partie de R1 dans R2 (R2 est rempli)


(defun actions (etat) 
  (let ((action nil))
   (if (< (car etat) 4) (push 1 action))
   (if (< (cadr etat) 3) (push 2 action))
   (if (> (car etat) 0) (push 3 action))
   (if (> (cadr etat) 0) (push 4 action))
   (if (and (<= (+ (car etat) (cadr etat)) 4) (> (cadr etat) 0))  (push 5 action))
   (if (and (<= (+ (car etat) (cadr etat)) 3) (> (car etat) 0)) (push 6 action))
   (if (and (> (+ (car etat) (cadr etat)) 4) (< (car etat) 4)) (push 7 action))
   (if (and (> (+ (car etat) (cadr etat)) 3) (< (cadr etat) 3)) (push 8 action))
   action))

(defun resultat-action (etat action)
  (cond
   ((= action 1) (list 4 (cadr etat)))
   ((= action 2) (list (car etat) 3))
   ((= action 3) (list 0 (cadr etat)))
   ((= action 4) (list (car etat) 0))
   ((= action 5) (list (+ (car etat) (cadr etat)) 0))
   ((= action 6) (list 0 (+ (car etat) (cadr etat))))
   ((= action 7) (list 4 (- (cadr etat) (- 4 (car etat)))))
   ((= action 8) (list (- (car etat) (- 3 (cadr etat))) 3))
    ))

(defun successeurs(etat etatsVisites)
  (let ((listeEtatsSuivants nil) 
        (listeAction (mapcar #' (lambda(x) (resultat-action etat x)) (actions etat))))
    
    (dolist (elem listeAction listeEtatsSuivants)
      (if (not (member elem etatsVisites :test #'equal))
          (push elem listeEtatsSuivants)
        ))
    ))


; Recherche en profondeur d'abord
;-------------------------------------------------------------------------

(defun rech-prof2(etat etatsVisites) 
  (let ((etatsSuivants (successeurs etat etatsVisites)))
    (push etat etatsVisites)
    (cond
     ((equal etatsSuivants nil) nil)
     ((= (car etat) 2) (format t "~&solution : ~a" (reverse etatsVisites)))
     (T 
      (while (not (equal etatsSuivants nil))
        (rech-prof2 (car etatsSuivants) etatsVisites)
        (pop etatsSuivants)
            )))))

(defun rech-prof(etat)
  (defparameter etatsVisites nil)
  
  (rech-prof2 etat etatsVisites))
  
;;(RECH-PROF '(0 0)) 


 
; Recherche en largeur d'abord
;-------------------------------------------------------------------------

(defun rech-larg (etat)
  (let (
       (file (list (list etat (list etat)))) 
       (temp nil) (etatsVisites nil) 
       (etatsSiuvants nil))
                         
  (while (not (null file))
    (progn
      (setq temp (pop file))
      (setq etat (car temp))
      (setq etatsVisites (cadr temp))
      (setq etatsSiuvants (successeurs etat etatsVisites))
      
      (if (equal (car etat) 2) ; si on a un état-solution sur cette branche
        (format t "~%Solution : ~a" etatsVisites) ; on affiche les états visités et on arrète le parcours sur ce noeud
        (dolist (elem etatsSiuvants) ;sinon pour chaque successeur de etat 
        (setq file (append file (list (list elem (append (list elem) etatsVisites) ))))
          ))))))

;;(RECH-LARG '(0 0)) 
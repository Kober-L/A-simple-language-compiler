;scorrimento memoria
 (defun execution-loop (state) 
  (cond ((string-equal (nth 0 state) 'halted-state) (nth 10 state)) 
       (T (execution-loop (one-instruction state)))))
        




;rilevamento tipo di istruzione
 (defun one-instruction (state)      
  (cond ((= (nth (ott-pc state) (ott-mem state)) 901) (inp state))
       ((= (nth (ott-pc state) (ott-mem state)) 902) (out state))                                                                                                    
       (T (let ((op (- (nth (ott-pc state) (ott-mem state)) 
                (mod (nth (ott-pc state) (ott-mem state)) 100))))                                                                                                                                                                                                                                                                         
            (cond 
             ((= op 100) (add state))
             ((= op 200) (sub state))
             ((= op 300) (sta state))                    
             ((= op 500) (lda state))
             ((= op 600) (bra state))
             ((= op 700) (brz state))
             ((= op 800) (brp state))
             ((= op 000) (hlt state)))))))
                                                           



; SUM
 (defun add (state) 

 (setf app (+ (ott-acc state) (nth (mod (nth (ott-pc state) 
 (ott-mem state)) 100) (ott-mem state))))                                                                        
 (if (string-equal (flag-control app) 'flag) 
    (setf (nth 12 state) 'flag)                                                    
    (setf (nth 12 state) 'noflag))
 (setf (nth 2 state) (mod app 1000))
 (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))
  state)
   
  


;SUB  
 (defun sub (state)

 (setf app (- (ott-acc state) 
 (nth (mod (nth (ott-pc state) (ott-mem state)) 100) (ott-mem state))))                                                                       
 (if (string-equal (flag-control app) 'flag) (setf (nth 12 state) 'flag)                                                    
     (setf (nth 12 state) 'noflag))
 (setf (nth 2 state) (mod app 1000))
 (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))
  state)
  
 

;STA  
 (defun sta (state)

 (setf (nth (mod (nth (ott-pc state) (ott-mem state)) 100) 
 (ott-mem state)) (ott-acc state))
 (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))
  state)
  



;BRZ 
 (defun brz (state)

 (if (and (= (nth 2 state) 0) (string-equal (nth 12 state) 'noflag))
    (setf (nth 4 state) (mod (nth (ott-pc state) (ott-mem state)) 100))
    (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))) 

    state)
        



;BRP
 (defun brp (state)

 (if (string-equal (nth 12 state) 'noflag)
    (setf (nth 4 state) (mod (nth (ott-pc state) (ott-mem state)) 100))
    (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100)))
     state)
        



;INP
 (defun inp (state)

 (if (null (ott-in state)) nil
 (progn (setf (nth 2 state) (nth 0 (nth 8 state)))
 (setf (nth 8 state) (rest (nth 8 state)))
 (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))
   state)))




;OUT 
 (defun out (state)

 (setf (nth 10 state) (append (ott-out state) (list (ott-acc state))))                                                        
 (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))
  state)

 

;HLT
 (defun hlt (state)
 (setf (nth 0 state) 'halted-state)
  state)                                           



;LDA 
 (defun lda (state)

 (setf (nth 2 state)
 (nth (mod (nth (ott-pc state) (ott-mem state)) 100) (ott-mem state)))
 (setf (nth 4 state) (mod (+ 1 (nth 4 state)) 100))
  state)



;BRA 
 (defun bra (state)
 (setf (nth 4 state) (mod (nth (ott-pc state) (ott-mem state)) 100))
  state)

     
                                                                                                     
 ;richiami 

 (defun ott-acc (state)
  (nth 2 state))
     

 (defun ott-pc (state)
  (nth 4 state))
     

 (defun ott-mem (state)
  (nth 6 state))
     

 (defun ott-in (state)
  (nth 8 state))
     

 (defun ott-out (state)
  (nth 10 state))
     

 (defun ott-flag (state)
  (nth 12 state))
     

; controllo flag
 (defun flag-control (app)

 (cond ((>= app 1000) 'flag)
       ((< app 0) 'flag)
       (T 'noflag)))
        







;Parser


;aprire il file e convertirlo in lista    

 (defun file-to-list (filename)

  (with-open-file (f filename :direction :input)
  (labels ((list ()                                                               
           (let ((line (read-line f nil nil)))                                                                       
             (when line (cons line (list))))))                                  
             (list))))                                                             
     

;converte caratteri della lista in maiuscolo e rimuove le stringhe del tipo ""
 (defun sistematina (list)
  (mapcar 'string-upcase (remove "" list :test #'equal)))



;elimina le stringhe contenenti solo spazi
 (defun del-emp-l (ind list)

  (cond ((null list) nil)
        ((= (list-length list) ind) list)
        ((all-spaces (explode (nth ind list)))
        (del-emp-l ind (remove-nth ind list)))   
        (T (del-emp-l (+ 1 ind) list))))
        

;dire se sono unicamente spazi vuoti
 (defun all-spaces (list)
  (cond ((null list) nil)
   ((and (= (list-length list) 1) 
     (string-equal (nth 0 list) " ")) t)
   ((and (= (list-length list) 1) 
     (not (string-equal (nth 0 list) " "))) nil)
   ((string-equal (first list) " ") (all-spaces (rest list)))))
       
  


;rimuovi l elmento all indice richiesto
 (defun remove-nth (ind list)
  (cond ((equal nil list) list)
        ((zerop ind) (cdr list))
  (t (cons (car list)
        (remove-nth (- ind 1) (cdr list))))))


;converti la stringa in una lista di char                    
 (defun explode (string)
    (mapcar #'(lambda (char) (intern (string (char-upcase char))))
    (coerce string 'list)))



;converti la lista di char in stringa
 (defun list-to-string (lst)
   (format nil "~{~A~}" lst))





;rimozione commenti
 (defun rimuovi-commento (ind list)
  (let ((app (rileva-commento 0 0 (explode (nth ind list)))))
   (cond 
    ((null app)  nil)
    ((= (list-length list) ind) list)
    ((= app  1) (rimuovi-commento (+ 1 ind) 
     (progn (setf (nth ind list) 
      (list-to-string (delete-from-n (rileva-ind 0 0 (explode (nth ind list)))
       (explode (nth ind list))))) list)))
    ((= app 0)  (rimuovi-commento (+ 1 ind) list)))))
      





  ;elimina a partire dall elemento n
 (defun delete-from-n (ind list)
  (if (>= ind (list-length list)) list
    (delete-from-n ind (remove-nth ind list))))
    
      

 ;rileva la presenza di un commento anche se mal formato
 (defun rileva-commento (ind cont list)   
  (let ((app (string-equal (nth ind list) "/")))                                     
   (cond ((>= ind (list-length list)) 0) 
        ((and app  (= cont 1)) 1)              
        ((and app  (= cont 0)) (rileva-commento (+ 1 ind) 1 list))
        ((and (not app) (= cont 1)) nil)
        ((and (not app) (= cont 0)) (rileva-commento (+ 1 ind) 0 list)))))



        
;rileva l' indice a cui si trova il commento
 (defun rileva-ind (ind cont list)
  (let ((app (string-equal (nth ind list) "/")))
   (cond  ((>= ind (list-length list)) (- ind 1)) 
        ((and app (= cont 1)) (- ind 1))              
        ((and app (= cont 0)) (rileva-ind (+ 1 ind) 1 list))
        ((and (not app) (= cont 0)) (rileva-ind (+ 1 ind) 0 list)))))
         

 

;scorre a la parte sx e rileva le label     
 (defun scor-sx-lbl-lst  (ind list labellist)
  (let ((app (sistematina (split-str (nth ind list)))))
   (cond ((= ind (list-length list)) labellist) 
      ((= (analizza app) 0)
      (scor-sx-lbl-lst (+ 1 ind) list labellist))                  
      ((= (analizza app) 1)
      (scor-sx-lbl-lst (+ 1 ind) list (append labellist (list (nth 0 app) ind)))))))
 
;rileva la presenza di una label
(defun analizza (list)
  (cond ((> (list-length list) 3) nil)
        ((and (= (list-length list) 3) (not (comp-inst (nth 0 list)))) 1)
        ((and (= (list-length list) 2) (not (comp-inst (nth 0 list)))) 1)
        ((= (list-length list) 2) 0)
        ((= (list-length list) 1) 0)))


(defun comp-inst (str)
 (if (or  (string-equal str "ADD") (string-equal str "SUB") 
          (string-equal str "STA") (string-equal str "LDA") 
          (string-equal str "BRA") (string-equal str "BRZ")
          (string-equal str "BRP") (string-equal str "INP") 
          (string-equal str "OUT") (string-equal str "HLT") 
          (string-equal str "DAT")) 
             t nil))


;rimuove le label gia rilevate a lato sx
 (defun rem-lbl (ind list)
  (let ((app (sistematina (split-str (nth ind list)))))
  (cond ((= ind (list-length list)) list) 
      ((= (analizza app) 0) 
      (rem-lbl (+ 1 ind) list))                  
      ((= (analizza app) 1) 
      (rem-lbl (+ 1 ind) (progn (setf (nth ind list) 
      (list-to-string (add-spaces (remove-nth 0 app)))) list))))))


;aggiunge " " in modo da non ottenere stringhe con parole "incollate"
 (defun add-spaces (list)
  (cond ((= (list-length list) 1) list)
       ((= (list-length list) 2) (list (nth 0 list) " " (nth 1 list)))))
     
 


;rileva le label sulla parte dx e gli assegna il dovuto valore
 (defun scor-dx-lbl-lst (ind labellist list)
  (let ((app (sistematina (split-str (nth ind list)))))
   (cond ((= ind (list-length list)) list) 
      ((= (list-length app) 1) (scor-dx-lbl-lst (+ 1 ind) labellist 
      (progn (setf (nth ind list) (list-to-string (add-spaces app))) list)))               
      ((= (list-length app) 2) (scor-dx-lbl-lst (+ 1 ind) labellist 
      (progn (setf (nth ind list)
      (list-to-string (add-spaces (studia-label labellist app)))) list)))
      ((= (list-length app) 3) (scor-dx-lbl-lst (+ 1 ind) labellist 
      (progn (setf (nth ind list)
      (list-to-string (add-spaces (studia-label labellist app)))) list))))))
                    



;rileva un numero o una label
 (defun studia-label (labellist list)
  (if (parse-integer (get-last list) :junk-allowed t) list
  (progn (setf (nth (- (list-length list) 1) list) 
  (scorri-lbllst 0 (get-last list) labellist)) list)))
  
                                                   


;cerca nella lista delle label rilevate prima la corrispondente
 (defun scorri-lbllst (ind element labellist)
  (let ((app (string-equal element (nth ind labellist))))
   (cond ((>= ind (list-length labellist)) nil)
        (app (setf element (nth (+ 1 ind) labellist)))
        ((not app) (scorri-lbllst (+ 2 ind) element labellist)))))              



;prende l ultimo elemento di una lista
 (defun get-last (list)
  (nth (- (list-length list) 1) list))





; metti una stringa in una lista "parola per parola"
 (defun split-str (string &optional (separator " "))
  (split-1 string separator))

 (defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string  :from-end t :test #'(lambda (x y)
       (find y x :test #'string=)))))
    (if n (split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))








;converte da istruzione letterale a numerica
 (defun convert (ind list)
 (let ((app (split-str (nth ind list))))
      
  (cond 
        ((= ind (list-length list)) list)
        ((and (> (list-length app) 1) (null (nth 1 app))) nil)  

        ((string-equal (nth 0 app) "ADD")
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
          (convert (+ 1 ind)
           (progn (setf (nth ind list) 
            (+ 100 (parse-integer (nth 1 app)  :junk-allowed t))) list))))

        ((string-equal (nth 0 app) "SUB")
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
          (convert (+ 1 ind) 
           (progn (setf (nth ind list)
            (+ 200 (parse-integer (nth 1 app)  :junk-allowed t))) list))))
 
        ((string-equal (nth 0 app) "STA")
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
          (convert (+ 1 ind)
           (progn (setf (nth ind list) 
            (+ 300 (parse-integer (nth 1 app)  :junk-allowed t))) list))))

        ((string-equal (nth 0 app) "LDA")
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
           (convert (+ 1 ind)
            (progn (setf (nth ind list) 
             (+ 500 (parse-integer (nth 1 app)  :junk-allowed t))) list))))

        ((string-equal (nth 0 app) "BRA")
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
          (convert (+ 1 ind)
           (progn (setf (nth ind list) 
            (+ 600 (parse-integer (nth 1 app)  :junk-allowed t))) list))))

        ((string-equal (nth 0 app) "BRZ") 
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
          (convert (+ 1 ind)
           (progn (setf (nth ind list) 
            (+ 700 (parse-integer (nth 1 app)  :junk-allowed t))) list))))

        ((string-equal (nth 0 app) "BRP")
         (if (>= (parse-integer (nth 1 app) :junk-allowed t) 100) nil
          (convert (+ 1 ind)
           (progn (setf (nth ind list) 
            (+ 800 (parse-integer (nth 1 app)  :junk-allowed t))) list))))

        ((string-equal (nth 0 app) "INP") 
         (if (/= (list-length app) 1) nil
          (convert (+ 1 ind)
           (progn (setf (nth ind list) 901) list))))

        ((string-equal (nth 0 app) "OUT")
         (if (/= (list-length app) 1) nil
          (convert (+ 1 ind) 
           (progn (setf (nth ind list) 902) list))))

        ((string-equal (nth 0 app) "HLT")
         (convert (+ 1 ind)
          (progn (setf (nth ind list) 000) list)))

        ((string-equal (nth 0 app) "DAT") 
         (if (= (list-length app) 1)
          (convert (+ 1 ind) (progn (setf (nth ind list) 000) list)) 
            (convert (+ 1 ind) (progn (setf (nth ind list) 
             (parse-integer (nth 1 app) :junk-allowed t)) list))))))) 







;rileviamo elementi di tipo NIL nella lista, per verificare la presenza di errori

 (defun check (ind list)
  (cond 
       ((= ind (list-length list)) list)
       ((null (nth ind list)) nil)
       ((not (null (nth ind list))) (check (+ 1 ind) list))))



;definizione della memoria 

 (defun def-mem (list)
  (cond ((= 100 (list-length list)) list)
       ((< 100 (list-length list)) nil)
       ((> 100 (list-length list)) 
       (append list 
       (make-list (- 100 (list-length list)) :initial-element 0)))))                                    





;gestione delle etichette
 (defun manage-label (list)
   (scor-dx-lbl-lst 0 (scor-sx-lbl-lst 0 list '()) 
   (sistematina (rem-lbl 0 list))))


 (defun lmc-load (filename)
     (def-mem (check 0 (convert 0 (manage-label (sistematina 
     (rimuovi-commento 0  (del-emp-l 0 (file-to-list filename)))))))))


       
 (defun lmc-run (filename inplist)
  (execution-loop 
   (list 'STATE :ACC 0 :PC 0 :MEM (lmc-load filename) :IN inplist :OUT NIL :FLAG 'noflag)))
 



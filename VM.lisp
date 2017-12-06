;---------------------------------------------------------------------------------------------------------------
;		Structure de la VM
;---------------------------------------------------------------------------------------------------------------

(defun make-vm (name memory_size)
	(if (<= memory_size 100)
		(error "too few memory")
		(progn
			;Déclaration de la memoire
			(setf (get name 'memory) (make-array memory_size :initial-element 0))
			(setf (get name 'memory-size) memory_size)			
			
			;Pile
			;base pointer (base de la pile/invariant)
			(setf (get name 'BP) -1)
			;mem-stack (haut de la pile/invariant)
			(setf (get name 'mem-stack) 99)
			;stack pointer (sommet de la pile)
			(setf (get name 'SP) -1)
			
			;Fin des instructions
			(setf (get name 'EOF) (+ (get name 'mem-stack) 1) )
			
			;Registres
			(setf (get name 'R0) 0)
			(setf (get name 'R1) 0)
			(setf (get name 'R2) 0)
			(setf (get name 'R3) 0)
			
			;frame pointer
			(setf (get name 'FP) -1)
			
			;programme pointer (ou compteur ordinal CO)
			(setf (get name 'PC) 0)
			
			;flag de comparaison
			(setf (get name 'FC) 0)
			(setf (get name 'FNIL) 0)
			
			;variable d'execution 
			(setf (get name 'halt) NIL)
			
			;retourne le nom de la vm créé
			name
		)
	)
)

;---------------------------------------------------------------------------------------------------------------
;		Jeu d'instructions
;---------------------------------------------------------------------------------------------------------------

;Instructions mémoire

(defun isRegister (reg)
	(or 
			(equal reg 'memory-size)
			(equal reg 'BP)
			(equal reg 'mem-stack)
			(equal reg 'SP)
			(equal reg 'EOF)
			(equal reg 'R0)
			(equal reg 'R1)
			(equal reg 'R2)
			(equal reg 'R3)
			(equal reg 'FP)
			(equal reg 'PC)
			(equal reg 'FC)
			(equal reg 'FNIL)
			(equal reg 'halt)
	)
)


(defun get-memory (vm address)
	(let ((tmp))
		(if (numberp address)
			(setq tmp address)
			(if (isRegister address) 
				(setq tmp (get vm address) )
				(error "~S is neither a register nor an address" address)
			)
		)
		(if (or (< tmp (get vm 'BP)) (> tmp (get vm 'memory-size)))
			(error "memory out of bound")
			(aref (get vm 'memory) tmp)
		)
	)
)

(defun set-memory (vm val address)
	(let ((tmp))
		(if (numberp address)
			(setq tmp address)
			(if (isRegister address) 
				(setq tmp (get vm address) )
				(error "~S is neither a register nor an address" address)
			)
		)
		(if (or (< tmp (get vm 'BP)) (> tmp (get vm 'memory-size)))
			(error "memory out of bound")
			(setf (aref (get vm 'memory) tmp) val)
		)
	)
)

(defun move-vm (vm reg1 reg2)
	(if (isRegister reg2)
		(let ((tmp))
			(if (isRegister reg1) 
				(setq tmp (get vm reg1) )
				(setq tmp reg1)
			)
			(setf (get vm reg2) tmp)
		)
		(error "~S is not a register" reg2)
	)
)

(defun load-vm (vm address reg) 
	(if (isRegister reg) 
		(move-vm vm (get-memory vm address) reg)
		(error "~S is not a register" reg)
	)
)

(defun store-vm (vm reg address)
	(if (isRegister reg) 
		(set-memory vm (get vm reg) address)
		(error "~S is not a register" reg)
	)
)

;Intructions de Pile

(defun push-vm (vm reg)
	(if (= (get vm 'SP) (get vm 'mem-stack) )
		(error "Stack Overflow")
		(progn
			(incr vm 'SP)
			(store-vm vm reg 'SP)
		)
	)
)

(defun pop-vm (vm reg)
	(if (> (get vm 'SP) (get vm 'BP))
		(progn
			(load-vm vm 'SP reg)
			(decr vm 'SP)
		)
		(error "Stack Underflow")
	)
)

;Opérations

(defun incr (vm reg &optional (i 1))
	(if (isRegister reg) 
		(if (numberp (get vm reg))
			(setf (get vm reg) (+ (get vm reg) i) )
			(error "~S content is not a number" reg)
		)
		(error "~S is not a register" reg)
	)
)

(defun decr (vm reg &optional (i 1))
	(if (isRegister reg) 
		(if (numberp (get vm reg))
			(setf (get vm reg) (- (get vm reg) i) )
			(error "~S content is not a number" reg)
		)
		(error "~S is not a register" reg)
	)
)

(defun add (vm reg1 reg2)
	(if (isRegister reg2)
		(if (numberp (get vm reg2))
			(let ((tmp))
				(if (numberp reg1)
					(setq tmp reg1)
					(if (numberp (get vm reg1))
						(setq tmp (get vm reg1))
						(error "~S content is not a number" reg1)
					)
				)
				(move-vm vm (+ (get vm reg2) tmp) reg2)
			)
			(error "~S content is not a number" reg2)
		)
		(error "~S is not a register" reg2)
	)
)

(defun sub (vm reg1 reg2)
	(if (isRegister reg2)
		(if (numberp (get vm reg2))
			(let ((tmp))
				(if (numberp reg1)
					(setq tmp reg1)
					(if (numberp (get vm reg1))
						(setq tmp (get vm reg1))
						(error "~S content is not a number" reg1)
					)
				)
				(move-vm vm (- (get vm reg2) tmp) reg2)
			)
			(error "~S content is not a number" reg2)
		)
		(error "~S is not a register" reg2)
	)
)

(defun mult (vm reg1 reg2)
	(if (isRegister reg2)
		(if (numberp (get vm reg2))
			(let ((tmp))
				(if (numberp reg1)
					(setq tmp reg1)
					(if (numberp (get vm reg1))
						(setq tmp (get vm reg1))
						(error "~S content is not a number" reg1)
					)
				)
				(move-vm vm (* (get vm reg2) tmp) reg2)
			)
			(error "~S content is not a number" reg2)
		)
		(error "~S is not a register" reg2)
	)
)

(defun div (vm reg1 reg2)
	(if (isRegister reg2)
		(if (numberp (get vm reg2))
			(if (= (get vm reg2) 0)
				(error "div by 0")
				(let ((tmp))
					(if (numberp reg1)
						(setq tmp reg1)
						(if (numberp (get vm reg1))
							(setq tmp (get vm reg1))
							(error "~S content is not a number" reg1)
						)
					)
					(move-vm vm (* (get vm reg2) tmp) reg2)
				)
			)
			(error "~S content is not a number" reg2)
		)
		(error "~S is not a register" reg2)
	)
)

;Comparaisons

(defun cmp (vm reg1 reg2)
	(if (isRegister reg2)
		(if (numberp (get vm reg2))
			(let ((tmp))
				(if (numberp reg1)
					(setq tmp reg1)
					(if (numberp (get vm reg1))
						(setq tmp (get vm reg1))
						(error "~S content is not a number" reg1)
					)
				)
				(if (= tmp (get vm reg2))
					(move-vm vm 0 'FC)
					(if(> tmp (get vm reg2))
						(move-vm vm 1 'FC)
						(move-vm vm -1 'FC)
					)
				)
			)
			(error "~S content is not a number" reg2)
		)
		(error "~S is not a register" reg2)
	)
)

(defun test-vm (vm reg)
	(if (isRegister reg)
		(if (equal (get vm reg) NIL)
			(move-vm vm 0 'FNIL)
			(move-vm vm 1 'FNIL)
		)
		(error "~S is not a register" reg)
	)
)

;Sauts

(defun jmp-vm (vm address)
	(move-vm vm address 'PC)
)

(defun jlt-vm (vm address)
	(if (= (get vm 'FC) -1)
		(jmp-vm vm address)
	)
)

(defun jeq-vm (vm address)
	(if (= (get vm 'FC) 0)
		(jmp-vm vm address)
	)
)

(defun jgt-vm (vm address)
	(if (= (get vm 'FC) 1)
		(jmp-vm vm address)
	)
)

(defun jle-vm (vm address)
	(if (or (= (get vm 'FC) -1) (= (get vm 'FC) 0))
		(jmp-vm vm address)
	)
)

(defun jge-vm (vm address)
	(if (or (= (get vm 'FC) 1) (= (get vm 'FC) 0))
		(jmp-vm vm address)
	)
)

(defun jne-vm (vm address)
	(if (not (= (get vm 'FC) 0))
		(jmp-vm vm address)
	)
)

(defun jtrue-vm (vm address)
	(if (= (get vm 'FNIL) 1)
		(jmp-vm vm address)
	)
)

(defun jnil-vm (vm address)
	(if (= (get vm 'FNIL) 0)
		(jmp-vm vm address)
	)
)

;Saut avec retour

(defun jsr-vm (vm address)
	(move-vm vm 'FP 'R2) ;Save du vieux FP
	(move-vm vm 'SP 'FP)	;Set FP aux params actuels

	(move-vm vm 'SP 'R3)	; Get du SP actuel pour recalculer le vieux SP
	(sub vm (get-memory vm 'FP) 'R3)	
	(decr vm 'R3)

	(push-vm vm 'R3)	;Push vieux SP
	(push-vm vm 'R2)	;Push vieux FP

	(push-vm vm 'PC)	;Push vieux PC

	(if (numberp address)

		(jmp-vm vm address)
		
		(let ((tmp) (i (get-memory vm 'FP)))
			(loop while (> i 0) 
			do
				(setq tmp (cons (get-memory vm (- (get vm 'FP) i) ) tmp) )
				(setq i (- i 1) )
			)
			(print tmp)
			(move-vm vm (apply address tmp) 'R0)
			(rtn-vm vm)
		)

	)
)

(defun rtn-vm (vm)
	(pop-vm vm 'PC) ;Récupération vieux PC
	(pop-vm vm 'FP)	;Récupération vieux FP
	(pop-vm vm 'R3)
	(move-vm vm 'R3 'SP)	;Récupération vieux SP
)

;Instructions utilitaires

(defun label-vm (vm label))

(defun nop ())

(defun halt (vm)
	(setf (get vm 'halt) T)
)

;---------------------------------------------------------------------------------------------------------------
;		Loader
;---------------------------------------------------------------------------------------------------------------

;Chargeur

(defun loader-vm (vm code)
	
	(setq LABELS (make-hash-table :size 0))
	(setq FORWARDS (make-hash-table :size 0))

	(let ((instr) (code code))
		(loop while (not (equal code NIL) )  ;Mise en mémoire et résolution des adresses connues
		do
			(progn
				(setq instr (car code) )
							
				(if (equal (car instr) 'label-vm)
				
					(progn													;THEN
						(if (nth-value 1 (gethash  (car (cdr (cdr instr) ) ) LABELS) )
							(error "Label already exist : ~S" (car (cdr (cdr instr) ) ))
							(setf (gethash (car (cdr (cdr instr) ) ) LABELS) (get vm 'EOF) )
						)
						(if (equal (car (cdr (cdr instr ) ) ) 'MAIN)
							(move-vm vm 'EOF 'PC)
						)
					)
					
					(progn												  ;ELSE
						(if (or
								(equal (car instr) 'jmp-vm)
								(equal (car instr) 'jlt-vm)
								(equal (car instr) 'jeq-vm)
								(equal (car instr) 'jgt-vm)
								(equal (car instr) 'jle-vm)
								(equal (car instr) 'jge-vm)
								(equal (car instr) 'jne-vm)
								(equal (car instr) 'jtrue-vm)
								(equal (car instr) 'jnil-vm)
								(equal (car instr) 'jsr-vm)						
							)
							(if (nth-value 1 (gethash  (car (cdr (cdr instr) ) ) LABELS) )
								(setf (car (cdr (cdr instr) ) ) (gethash  (car (cdr (cdr instr) ) ) LABELS) )
								(setf (gethash (get vm 'EOF) FORWARDS) (car (cdr (cdr instr) ) ) ) 
							)
						)					
					)
				)
				(set-memory vm instr (get vm 'EOF))
				(incr vm 'EOF)
				(setf code (cdr code))					
			)		
		)
	)
	
	(maphash 
		#'(lambda (key value)
			(if (nth-value 1 (gethash value LABELS) )
				(setf
					(car (cdr (cdr (get-memory vm key) ) ) )
					(gethash value LABELS)
				)
				(error "Undefined Label : ~S" value )
			)
		) 
		FORWARDS	
	)

	(get vm 'memory)	
)

;---------------------------------------------------------------------------------------------------------------
;		Exécution
;---------------------------------------------------------------------------------------------------------------

;Execution du code assembleur

(defun show-instr (vm instr &optional (pileElem 20))
	(format 
		t 
		"Reg : [PC : ~S, SP : ~S, FP : ~S, FC : ~S, R0 : ~S, R1 : ~S, R2 : ~S, R3 : ~S]~%Pile : [" 
		(get vm 'PC) 
		(get vm 'SP)
		(get vm 'FP) 
		(get vm 'FC)
		(get vm 'R0) 
		(get vm 'R1) 
		(get vm 'R2) 
		(get vm 'R3)
	)
	(dotimes (i pileElem)
		(if (< i (- pileElem 1))
			(format t "~S, " (get-memory vm i))
			(format t "~S, ..." (get-memory vm i))
		)
	)
	(format t "]~%~%~S~%" instr)
)

(defun show-instruction-list (vm)
	(loop for i from (+ 1 (get vm 'mem-stack)) to (- (get vm 'EOF) 1)
	do
		(format t "~S : ~S~%" i (get-memory vm i))
	)
)

(defun apply-vm (vm instr &optional (show NIL))
	(if show
		(show-instr vm instr)
	)
	(apply 
		(car instr) 
		(mapcar 
			#'(lambda (x)
				(if (listp x)
				(apply-vm vm x)
				x
				)	
			) 
			(cdr instr)
		)
	)	
)

(defun exec-vm (vm &optional (show NIL))
	(if (= (get vm 'PC) 0)
		(error "no main function")
		(progn
			(if show
				(progn
					(show-instruction-list vm)
					(format t "---------------------~%(RUN VM)~%")
				)
			)
			(loop while (and (< (get vm 'PC) (get vm 'EOF)) (not (get vm 'halt) ) )  ;exécution du code en mémoire
			do
				(apply-vm vm (get-memory vm (get vm 'PC)) show)
				(incr vm 'PC)
			)
		)
	)
	(get vm 'R0)
)

;---------------------------------------------------------------------------------------------------------------
;		Tests de la VM et de l'Assembleur
;---------------------------------------------------------------------------------------------------------------

(make-vm 'vm 1000)

(setq ref 
	'(
		(label-vm vm MAIN) 
		(move-vm vm 0 R0) 
		(move-vm vm -1 R1) 
		(cmp vm R0 R1) 
		(jeq-vm vm TRUE) 
		(jle-vm vm FALSE) 
		(push-vm vm 1) 
		(halt vm) 
		(label-vm vm TRUE) 
		(push-vm vm 0) 
		(jmp-vm vm EOF) 
		(label-vm vm FALSE) 
		(push-vm vm -1) 
		(label-vm vm EOF)
	)
)

(setq fact
	'(
		(label-vm vm FACT)

		(move-vm vm 1 R0)
		(move-vm vm FP R1)
		(decr vm R1)
		(load-vm vm R1 R2)

		(label-vm vm BOUCLE)
		(mult vm R2 R0)
		(decr vm R2)
		(cmp vm 1 R2)
		(jlt-vm vm BOUCLE)
		(rtn-vm vm)

		(label-vm vm MAIN)

		(move-vm vm 5 R0)
		(push-vm vm R0)	;Push n1
		(move-vm vm 1 R0)
		(push-vm vm R0)	;Push nb param

		(jsr-vm vm FACT)

		(label-vm vm EOF)
	)
)

(setq fiboNT
	'(
		(label-vm vm FIBO)

		;Récupération de la valeure de n1

		(move-vm vm FP R1)
		(decr vm R1) ;Récupération de l'adresse de la variable locale n1 (FC-1)
		(load-vm vm R1 R1) ;Récupération de la variable locale n1


		;Conditions d'arrêt

		;n=0
		(move-vm vm 0 R0)
		(cmp vm 0 R1)
		(jeq-vm vm END_FIBO)

		;n=1
		(move-vm vm 1 R0)
		(cmp vm 1 R1)
		(jeq-vm vm END_FIBO)

		;Appel Fibo(n-1)

		(push-vm vm R1) ;Save du n actuel

		(sub vm 1 R1) ;n-1
		
		(push-vm vm R1)	;Push n1
		(move-vm vm 1 R1)
		(push-vm vm R1)	;Push nb param

		(jsr-vm vm FIBO)

		(pop-vm vm R1) ;Récupération du n
		
		(push-vm vm R0) ;Save de Fibo(n-1)

		;Appel Fibo(n-2)

		(sub vm 2 R1) ;n-2
		
		(push-vm vm R1)	;Push n1
		(move-vm vm 1 R1)
		(push-vm vm R1)	;Push nb param

		(jsr-vm vm FIBO)

		(pop-vm vm R1)  ;Pop de Fibo(n-1) dans R1

		(add vm R1 R0) ; Fibo (n-1) + Fibo (n-2) dans R0

		(label-vm vm END_FIBO)

		(rtn-vm vm)

		(label-vm vm MAIN)
		
		(move-vm vm 4 R0)
		(push-vm vm R0)	;Push n1
		(move-vm vm 1 R0)
		(push-vm vm R0)	;Push nb param

		(jsr-vm vm FIBO)

		(label-vm vm EOF)
	)	

)

(defun fact (x)
	(if (= x 0)
		1
		(* x (fact (- x 1)))
	)
)

(setq testJSR 
	'(
		(label-vm vm MAIN)
		(move-vm vm 5 R1)
		(push-vm vm R1)
		(move-vm vm 1 R1)
		(push-vm vm R1)
		(jsr-vm vm fact)
	)
)


(set 'code fiboNT)


;---------------------------------------------------------------------------------------------------------------
;		Compilateur
;---------------------------------------------------------------------------------------------------------------




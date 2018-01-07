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
			(setf (get name 'BP) 0)
			;mem-stack (haut de la pile/invariant)
			(setf (get name 'mem-stack) 100)
			;stack pointer (sommet de la pile)
			(setf (get name 'SP) 0)
			
			;Fin des instructions
			(setf (get name 'EOF) (+ (get name 'mem-stack) 1) )
			
			;Registres
			(setf (get name 'R0) 0)
			(setf (get name 'R1) 0)
			(setf (get name 'R2) 0)
			(setf (get name 'R3) 0)

			;Hash-map globale
			(setf (get name 'GLOBALS) (make-hash-table :size 0) )
			
			;frame pointer
			(setf (get name 'FP) 0)
			
			;programme counter (ou compteur ordinal CO)
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
		
		(if (nth-value 1 (gethash  address (get vm 'GLOBALS) ) )
			(jmp-vm vm (gethash  address (get vm 'GLOBALS) ) )
			(error "Undefined function : ~S" address)
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

(defun loader-function (vm code)

	(let ((tmp code) (LABELS) (FORWARDS))

		(setq LABELS (make-hash-table :size 0))
		(setq FORWARDS (make-hash-table :size 0))

		(let ((first_instr))
			(setq first_instr (car tmp) )
			(if (equal (car first_instr) 'LABEL)
				(if (nth-value 1 (gethash  (car (cdr first_instr)  ) (get vm 'GLOBALS) ) )
					(error "Function already exist : ~S" (car (cdr first_instr) ) )
					(progn
						(setf (gethash (car (cdr first_instr) ) (get vm 'GLOBALS) ) (get vm 'EOF) )
						(set-memory vm first_instr (get vm 'EOF))
						(incr vm 'EOF)
						(setf tmp (cdr tmp))
					)
				)
				(error "First instruction must be the LABEL of the function")
			)
		)

		(loop while (not (equal tmp NIL) )  ;Mise en mémoire et résolution des adresses connues
		do
			(let ((instr))
				(setq instr (copy-list (car tmp) ) )
							
				(if (equal (car instr) 'LABEL)
				
					      													;THEN
					(if (nth-value 1 (gethash  (car (cdr instr)  ) LABELS) )
						(error "Label already exist : ~S" (car (cdr instr) ))
						(setf (gethash (car (cdr instr) ) LABELS) (get vm 'EOF) )
					)
					
					(progn												  ;ELSE
						(if (or
								(equal (car instr) 'JMP)
								(equal (car instr) 'JLT)
								(equal (car instr) 'JEQ)
								(equal (car instr) 'JGT)
								(equal (car instr) 'JLE)
								(equal (car instr) 'JGE)
								(equal (car instr) 'JNE)
								(equal (car instr) 'JTRUE)
								(equal (car instr) 'JNIL)					
							)
							(if (nth-value 1 (gethash  (car (cdr instr) ) LABELS) )
								(setf (car (cdr instr) ) (gethash  (car (cdr instr) ) LABELS) )
								(setf (gethash (get vm 'EOF) FORWARDS) (car (cdr instr) ) ) 
							)
						)					
					)
				)

				(set-memory vm instr (get vm 'EOF))
				(incr vm 'EOF)
				(setf tmp (cdr tmp))					
			)		
		)
	
		(maphash 
			#'(lambda (key value)
				(if (nth-value 1 (gethash value LABELS) )
					(setf
						(car (cdr (get-memory vm key) ) )
						(gethash value LABELS)
					)
					(error "Undefined Label : ~S" value )
				)
			) 
			FORWARDS	
		)

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
	(loop for i from (+ (get vm 'mem-stack) 1) to (- (get vm 'EOF) 1)
	do
		(format t "~S : ~S~%" i (get-memory vm i))
	)
	(format t "---------------------~%(RUN VM)~%")
)

(defun eval-vm (vm instr &optional (show NIL))
	(if show
		(show-instr vm instr)
	)
	(let ((call))
		(case (car instr)
			( 'MOVE (setq call 'move-vm) )
			( 'LOAD (setq call 'load-vm) )
			( 'STORE (setq call 'store-vm) )
			( 'PUSH (setq call 'push-vm) )
			( 'POP (setq call 'pop-vm) )
			( 'INCR (setq call 'incr) )
			( 'DECR (setq call 'decr) )
			( 'ADD (setq call 'add) )
			( 'SUB (setq call 'sub) )
			( 'MULT (setq call 'mult) )
			( 'DIV (setq call 'div) )
			( 'CMP (setq call 'cmp) )
			( 'TEST (setq call 'test-vm) )
			( 'JMP (setq call 'jmp-vm) )
			( 'JLT (setq call 'jlt-vm) )
			( 'JEQ (setq call 'jeq-vm) )
			( 'JGT (setq call 'jgt-vm) )
			( 'JLE (setq call 'jle-vm) )
			( 'JGE (setq call 'jge-vm) )
			( 'JNE (setq call 'jne-vm) )
			( 'JTRUE (setq call 'jtrue-vm) )
			( 'JNIL (setq call 'jnil-vm) )
			( 'JSR (setq call 'jsr-vm) )
			( 'RTN (setq call 'rtn-vm) )
			( 'LABEL (setq call 'label-vm) )
			( 'NOP (setq call 'nop) )
			( 'HALT (setq call 'halt) )
		)
		
		(if (equal call NIL)
			(error "~S : Instruction inconnue" (car instr) )
			(apply call (cons vm (cdr instr)))
		)
	)
	;(apply 
		;(car instr) 
		;(mapcar 
			;#'(lambda (x)
				;(if (listp x)
				;(eval-vm vm x)
				;x
				;)	
			;) 
			;(cdr instr)
		;)
	;)	
)

(defun vm-apply (vm label param &optional (show NIL))
	(if (nth-value 1 (gethash label (get vm 'GLOBALS) ) )
		(progn

			(if (and show (not (equal param NIL) ) )
				(format t "(VM-APPLY) push des arguments et du nb arguments~%")
			)
			(dolist 
				(x param NIL) 
				(progn
					(if show
						(format t "(MOVE ~S R0)~%(PUSH R0)~%" x)
					)
					(move-vm vm x 'R0)
					(push-vm vm 'R0)
				)  
			)

			(if (not (equal param NIL))
				(progn
					(if show
						(format t "(MOVE ~S R0)~%(PUSH R0)~%(JSR ~S)~%---------------------~%" 
							(list-length param) label )
					)
					(move-vm vm (list-length param) 'R0)
					(push-vm vm 'R0)
				)
			)

			(if show
				(show-instruction-list vm)
			)

			(move-vm vm 'EOF 'PC)
			(jsr-vm vm label)

			(loop while (and (< (get vm 'PC) (get vm 'EOF)) (not (get vm 'halt) ) )  ;exécution du code en mémoire
			do
				(eval-vm vm (get-memory vm (get vm 'PC)) show)
				(incr vm 'PC)
			)
		)
		(error "Undefined function : ~S" label)
	)
	(get vm 'R0)
)

(defun exec-vm (vm &optional (show NIL))
	(vm-apply vm 'MAIN '() show)
)

;---------------------------------------------------------------------------------------------------------------
;		Tests de la VM et de l'Assembleur
;---------------------------------------------------------------------------------------------------------------

(make-vm 'vm 1000)

(setq ref 
	'(
		(LABEL MAIN) 
		(MOVE 0 R0) 
		(MOVE -1 R1) 
		(CMP R0 R1) 
		(JEQ TRUE) 
		(JLE FALSE)
		(MOVE 1 R2) 
		(PUSH R2) 
		(HALT) 
		(LABEL TRUE) 
		(MOVE 0 R2)
		(PUSH R2) 
		(JMP EOF) 
		(LABEL FALSE)
		(MOVE -1 R2) 
		(PUSH R2) 
		(LABEL EOF)
	)
)

(setq fact
	'(
		(LABEL FACT)

		(MOVE 1 R0)
		(MOVE FP R1)
		(DECR R1)
		(LOAD R1 R2)

		(LABEL BOUCLE)
		(MULT R2 R0)
		(DECR R2)
		(CMP 0 R2)
		(JLT BOUCLE)
		(RTN)
	)
)

(setq fiboNT
	'(
		(LABEL FIBO)

		;Récupération de la valeure de n1

		(MOVE FP R1)
		(DECR R1) ;Récupération de l'adresse de la variable locale n1 (FC-1)
		(LOAD R1 R1) ;Récupération de la variable locale n1


		;Conditions d'arrêt

		;n=0
		(MOVE 0 R0)
		(CMP 0 R1)
		(JEQ END_FIBO)

		;n=1
		(MOVE 1 R0)
		(CMP 1 R1)
		(JEQ END_FIBO)

		;Appel Fibo(n-1)

		(PUSH R1) ;Save du n actuel

		(SUB 1 R1) ;n-1
		
		(PUSH R1)	;Push n1
		(MOVE 1 R1)
		(PUSH R1)	;Push nb param

		(JSR FIBO)

		(POP R1) ;Récupération du n
		
		(PUSH R0) ;Save de Fibo(n-1)

		;Appel Fibo(n-2)

		(SUB 2 R1) ;n-2
		
		(PUSH R1)	;Push n1
		(MOVE 1 R1)
		(PUSH R1)	;Push nb param

		(JSR FIBO)

		(POP R1)  ;Pop de Fibo(n-1) dans R1

		(ADD R1 R0) ; Fibo (n-1) + Fibo (n-2) dans R0

		(LABEL END_FIBO)

		(RTN)
	)	

)

(setq MAINFACT
	'(
		(LABEL MAIN)

		(MOVE 5 R0)	;Push de l'argument de Factorielle
		(PUSH R0)	;Push n1
		(MOVE 1 R0)
		(PUSH R0)	;Push nb param

		(JSR FACT)

		(LABEL EOF)
	)
)

(setq MAINFIBO
	'(
		(LABEL MAIN)
		
		(MOVE 6 R0)
		(PUSH R0)	;Push n1
		(MOVE 1 R0)
		(PUSH R0)	;Push nb param

		(JSR FIBO)

		(LABEL EOF)
	)
)

;---------------------------------------------------------------------------------------------------------------
;		Compilateur
;---------------------------------------------------------------------------------------------------------------




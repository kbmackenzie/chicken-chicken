(module (chicken-chicken compiler) (compile inspect)
  (import scheme (chicken base) (chicken string) (chicken format) srfi-13 monad)
  (import (chicken-chicken vm) (chicken-chicken utils))

  (define chicken-string  "chicken")
  (define chicken-length  (string-length chicken-string))
  (define chicken-letters (string->list "chicken"))

  ; ------------------------
  ; Instructions:
  ; ------------------------

  (define-record-type :instruction
    (instruction opcode operand)
    instruction?
    (opcode instruction-opcode)
    (operand instruction-operand))

  ; Instructions can be represented purely as integer numbers from 0 to infinity.
  ; Any number greater than 9 is considered a 'push' instruction, however.
  ;
  ; With 'push' instructions, the value to be pushed to the stack is calculated as: 
  ;   f(x) = x - 10
  ; Where x is the numeric value of the instruction.

  ; axe:      exit program
  ; chicken:  push the string "chicken" onto the stack
  ; add:      add two top stack values
  ; fox:      subtract two top stack values
  ; rooster:  multiply two top stack values
  ; compare:  test equality of two top stack values, push result
  ; pick:     double-wide, load value from stack or user input by index. too complex to sum up
  ; peck:     store value from stack in another address. too complex to sum up.
  ; fr:       jump instruction; self-explanatory. moves the instruction pointer.
  ; BBQ:      pop x from stack, push &#x (tl;dr: html-escape char)
  ; push:     pushes number to stack; already explained above

  ; Asks if an instruction has an operand.
  ; returns: boolean
  (define (has-operand? instr)
    (not (eqv? (instruction-operand instr) #f)))

  ; Convert an instruction to a string.
  ; returns: string
  (define (instruction->string instr)
    (case (instruction-opcode instr)
      ((0) "axe"     )
      ((1) "chicken" )
      ((2) "add"     )
      ((3) "fox"     )
      ((4) "rooster" )
      ((5) "compare" )
      ((6) (sprintf "pick ~A" (instruction-operand instr)))
      ((7) "peck"    )
      ((8) "fr"      )
      ((9) "BBQ"     )
      (else => (lambda (n) (sprintf "push ~A" (- n 10))))))

  ; ------------------------
  ; Parsing Chicken:
  ; ------------------------

  ; Is the character at the specified position a space?
  ; returns: boolean
  (define (is-space str position)
    (and
      (< position (string-length str))
      (eqv? (string-ref str position) #\ )))

  ; Get index of the start of the next token after position.
  ; returns: number
  (define (next-token str position)
    (if (is-space str position) (next-token str (+ 1 position)) position))

  ; Does the input string have the word 'chicken' at the specified position?
  ; returns: boolean
  (define (contains-chicken str position)
    (string=
      str
      chicken-string
      position
      (min (+ position chicken-length) (string-length str))))

  ; Utility for generating syntax error messages.
  ; returns: string
  (define (syntax-error line position)
    (sprintf "line ~A: unrecognized character at ~A: ~A"
      (+ (car line) 1)
      position
      (string-ref (cadr line) position)))

  ; Count the amount of 'chicken's in a given line.
  ; returns: number
  (define (count-chickens line) 
    (letrec
      ((line-content (cadr line))
       (line-length  (string-length (cadr line)))
       (count
         (lambda (chickens position)
           (cond
             ((contains-chicken line-content position) (count (+ 1 chickens) (+ position chicken-length))  )
             ((is-space line-content position)         (count chickens (next-token line-content position)) )
             ((>= position line-length)                (<either>-unit chickens)                            )
             (else                                     (<either>-fail (syntax-error line position))))))    )
      (count 0 0)))

  ; Parse a single instruction.
  ; returns: instruction
  (define (parse-instruction lines)
    (do-using <either>
      (opcode  <- (count-chickens (car lines)))
      (operand <- (if (= opcode 6) (parse-operand (cdr lines)) (return #f)))
      (return (instruction opcode operand))))

  ; Parse an operand for the 'pick' instruction.
  ; returns: number
  (define (parse-operand lines)
    (do-using <either>
      (if (null? lines)
        (fail "expected operand; got end of input")
        (count-chickens (car lines)))))

  ; Parse all instructions.
  ; returns: list of instructions
  (define (parse-instructions lines)
    (if (null? lines)
      (<either>-unit '())
      (do-using <either>
        (instr <- (parse-instruction lines))
        (rest  <- (parse-instructions (if (has-operand? instr) (cddr lines) (cdr lines))))
        (return (cons instr rest)))))

  ; ------------------------
  ; Compiling Chicken:
  ; ------------------------

  ; Convert a list of instructions into a list of integers.
  ; returns: list of numbers
  (define (instructions->integers instrs)
    (concat-map
      (lambda (instr)
        (if (has-operand? instr)
          (list (instruction-opcode instr) (instruction-operand instr))
          (list (instruction-opcode instr))))
      instrs))

  ; Compile lines into ES2016-compliant JavaScript.
  ; returns: string
  (define (compile lines)
    (do-using <either>
      (instructions <- (parse-instructions (enumerate-lines lines)))
      (let* ((integers (instructions->integers instructions))
             (js-array (string-join (map number->string integers) ","))
             (output   (sprintf "~A;const instructions=[~A];" vm js-array)))
        (return output))))

  ; Parse instructions and return list of pretty-printed instruction names.
  ; returns: list of strings
  (define (inspect lines)
    (do-using <either>
      (instructions <- (parse-instructions (enumerate-lines lines)))
      (return (map instruction->string instructions))))
)

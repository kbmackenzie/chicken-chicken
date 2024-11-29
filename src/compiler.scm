(module (chicken-chicken compiler) (compile)
  (import scheme (chicken base) (chicken string) (chicken format) srfi-1 srfi-13 monad)
  (import (chicken-chicken vm))

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
  ; pick:     double-long, load value from stack or user input by index. too complex to sum up
  ; peck:     store value from stack in another address. too complex to sum up.
  ; fr:       jump instruction; self-explanatory. moves the instruction pointer.
  ; BBQ:      pop x from stack, push &#x (tl;dr: html-escape char)
  ; push:     pushes number to stack; already explained above

  ; A little util for later.
  (define (has-operand? instr)
    (not (eqv? (instruction-operand instr) #f)))

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
  (define (is-space str position)
    (and
      (< position (string-length str))
      (eqv? (string-ref str position) #\ )))

  (define (skip-spaces str position)
    (if (is-space str position) (skip-spaces str (+ 1 position)) position))

  (define (contains-chicken str at-position)
    (letrec
      ((compare-letters
         (lambda (letters position)
           (if (null? letters)
             #t
             (and
               (< position (string-length str))
               (eqv? (string-ref str position) (car letters))
               (compare-letters (cdr letters) (+ 1 position)))))))
      (compare-letters chicken-letters at-position)))

  (define (generate-error line position)
    (sprintf "line ~A: unrecognized character at ~A: ~A"
      (+ (car line) 1)
      position
      (string-ref (cadr line) position)))

  (define (count-chicken line) 
    (letrec
      ((line-content (cadr line))
       (line-length  (string-length (cadr line)))
       (count
         (lambda (chickens position)
           (cond
             ((contains-chicken line-content position)
                (count (+ 1 chickens) (+ position chicken-length)))
             ((is-space line-content position)
                (count chickens (skip-spaces line-content position)))
             ((>= position line-length)
                (<either>-unit chickens))
             (else
               (<either>-fail (generate-error line position)))))))
      (count 0 0)))

  (define (parse-instruction lines)
    (do-using <either>
      (opcode  <- (count-chicken (car lines)))
      (operand <- (if (= opcode 6) (parse-operand (cdr lines)) (return #f)))
      (return (instruction opcode operand))))

  (define (parse-operand lines)
    (do-using <either>
      (if (null? lines)
        (fail "expected operand; got none")
        (count-chicken (car lines)))))

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
  (define (flat-map fn xs)
    (flatten (map fn xs)))

  (define (enumerate-lines lines)
    (zip (iota (length lines)) lines))

  (define (instructions->integers instrs)
    (flat-map
      (lambda (instr)
        (if (has-operand? instrs)
          (list (instruction-opcode instr) (instruction-operand instr))
          (list (instruction-opcode instr))))
      instrs))

  (define (compile lines)
    (do-using <either>
      (instructions <- (parse-instructions lines))
      (let* ((integers  (instructions->integers instructions))
             (js-array  (string-join (map string integers) ","))
             (output    (sprintf "~S;\nconst instructions=[~S];\n" vm js-array)))
        (return output))))
)

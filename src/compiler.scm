(module (chicken-chicken compiler) (compile inspect chicken-options)
  (import scheme (chicken base) (chicken string) (chicken format) srfi-1 srfi-13 monad)
  (import (chicken-chicken vm) (chicken-chicken utils))

  ; ------------------------
  ; Options:
  ; ------------------------
  (define-record-type :chicken-options
    (chicken-options mode)
    chicken-options?
    (mode chicken-options-mode))

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

  ; For this parser, a line is a tuple containing:
  ;   1st value - line number
  ;   2nd value - line content
  ; The line number is important for helpful error messages!

  ; Utility for generating syntax error messages.
  ; returns: string
  (define (syntax-error line token)
    (sprintf "unrecognized token in line ~A: ~A" (+ (car line) 1) token))

  ; Count the amount of 'chicken's in a given line.
  ; returns: either string number
  (define (count-chickens line)
    (fold
      (lambda (token acc)
        (do-using <either>
          (count <- acc)
          (if (string=? token "chicken")
            (return (+ 1 count))
            (fail (syntax-error line token)))))
      (<either>-unit 0)
      (string-tokenize (cadr line))))

  ; Parse a single instruction.
  ; returns: either string instruction
  (define (parse-instruction lines)
    (do-using <either>
      (opcode  <- (count-chickens (car lines)))
      (operand <- (if (= opcode 6) (parse-operand (cdr lines)) (return #f)))
      (return (instruction opcode operand))))

  ; Parse an operand for the 'pick' instruction.
  ; returns: either string number
  (define (parse-operand lines)
    (do-using <either>
      (if (null? lines)
        (fail "expected operand; got end of input")
        (count-chickens (car lines)))))

  ; Parse all instructions.
  ; returns: either string (list of instructions)
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
  ; returns: either string (list of numbers)
  (define (instructions->integers instrs)
    (concat-map
      (lambda (instr)
        (if (has-operand? instr)
          (list (instruction-opcode instr) (instruction-operand instr))
          (list (instruction-opcode instr))))
      instrs))

  ; Strict pragma for ES6 JavaScript.
  (define strict-pragma "'use strict'")

  ; Default export lambda expression. 
  (define export-lambda "function(input, options) { chicken(instructions, input, options); }")

  ; Generate module export based on 'mode' option.
  ; When it receives an invalid mode, it does nothing other than add a semicolon.
  (define (generate-module mode code)
    (case mode
      (('es 'es6 'esmodule) (sprintf "~A;export default ~A;"         code export-lambda))
      (('node 'commonjs)    (sprintf "~A;module.exports = ~A;"       code export-lambda))
      (('global 'vanilla)   (sprintf "{~A;globalThis.chicken = ~A;}" code export-lambda))
      (('iife 'run 'exec)   (sprintf "~A;(function() { ~A; })();"    code export-lambda))
      (else                 (string-append code ";"))))

  ; Compile lines into ES2016-compliant JavaScript.
  ; returns: either string string
  (define (compile lines options)
    (do-using <either>
      (instructions <- (parse-instructions (enumerate-lines lines)))
      (let* ((integers (instructions->integers instructions))
             (js-array (string-join (map number->string integers) ","))
             (mode     (and (chicken-options? options) (chicken-options-mode options)))
             (code     (sprintf "~A;const instructions=[~A]" vm js-array))
             (module_  (generate-module mode code)))
        (return (string-append "'use strict';" module_)))))

  ; Parse instructions and return list of pretty-printed instruction names.
  ; returns: either string (list of strings)
  (define (inspect lines)
    (do-using <either>
      (instructions <- (parse-instructions (enumerate-lines lines)))
      (return (map instruction->string instructions))))
)

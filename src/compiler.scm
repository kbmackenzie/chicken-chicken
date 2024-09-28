(module chicken-in-chicken
  (parser?
   parser-value
   parser-result
   parser-success
   parser-failure
   parser-success?
   parser-failure?
   parse-instructions
   instruction->string
  )

  (import scheme (chicken base) (chicken string) (chicken format) srfi-13 monad)

  (define chicken-string  "chicken")
  (define chicken-length  (string-length chicken-string))
  (define chicken-letters (string->list "chicken"))

  ; ------------------------
  ; Parser:
  ; ------------------------
  (define-record-type :parser
    (parser result value)
    parser? 
    (result parser-result)
    (value  parser-value))

  (define (parser-success value)
    (parser 'success value))

  (define (parser-failure value)
    (parser 'failure value))

  (define (parser-success? p)
    (and (parser? p) (eqv? (parser-result p) 'success)))

  (define (parser-failure? p)
    (and (parser? p) (eqv? (parser-result p) 'failure)))

  ; Monadic operations.
  ; The parser type is basically just an Either monad!

  (define-monad
    <parser>
    (lambda (a)   ; pure
      (parser-success a))
    (lambda (m f) ; bind
      (if (parser-failure? m) m (f (parser-value m))))
    (lambda (e)   ; fail
      (parser-failure e)))

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
  (define (has-operand? instruction)
    (not (eqv? (instruction-operand instruction) #f)))

  (define (instruction->string instruction)
    (case (instruction-opcode instruction)
      ((0) "axe"     )
      ((1) "chicken" )
      ((2) "add"     )
      ((3) "fox"     )
      ((4) "rooster" )
      ((5) "compare" )
      ((6) (sprintf "pick ~A" (instruction-operand instruction)))
      ((7) "peck"    )
      ((8) "fr"      )
      ((9) "BBQ"     )
      (else => (lambda (n) (sprintf "push ~A" (- n 10))))))

  ; ------------------------
  ; Parsing Chicken:
  ; ------------------------
  (define (is-space str pos)
    (and
      (< pos (string-length str))
      (eqv? (string-ref str pos) #\ )))

  (define (skip-spaces str pos)
    (if (is-space str pos) (skip-spaces str (+ 1 pos)) pos))

  (define (is-chicken str start-pos)
    (define len (string-length str))
    (define (compare-letters letters pos)
      (if (null? letters)
        #t
        (and
          (< pos len)
          (eqv? (string-ref str pos) (car letters))
          (compare-letters (cdr letters) (+ 1 pos)))))
    (compare-letters chicken-letters start-pos))

  (define (count-chicken line) 
    (define len (string-length line))
    (define (count num pos)
      (cond
        ((is-chicken line pos) (count (+ 1 num) (+ pos chicken-length)))
        ((is-space line pos)   (count num (skip-spaces line pos)))
        ((>= pos len)          (parser-success num))
        (else
          (let ((unrecognized (string (string-ref line pos)))
                (message (sprintf "unrecognized character at ~A: ~S" pos unrecognized)))
            (parser-failure message)))))
    (count 0 0))

  (define (parse-instruction lines)
    (do-using <parser>
      (opcode  <- (count-chicken (car lines)))
      (operand <- (if (= opcode 6) (parse-operand (cdr lines)) (return #f)))
      (return (instruction opcode operand))))

  (define (parse-operand lines)
    (do-using <parser>
      (if (null? lines)
        (fail "expected operand; got none")
        (count-chicken (car lines)))))

  (define (parse-instructions lines)
    (if (null? lines)
      (parser-success '())
      (do-using <parser>
        (instruction <- (parse-instruction lines))
        (rest        <- (parse-instructions (if (has-operand? instruction) (cddr lines) (cdr lines))))
        (return (cons instruction rest)))))
)

(module chicken-in-chicken
  (parser?
   parser-value
   parser-result
   parser-success
   parser-failure
   parser-success?
   parser-failure?
   parse-instructions
   show-instruction
   show-instructions
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
          (let ()
            (define unrecognized (string (string-ref line pos)))
            (define message (sprintf "unrecognized character at ~A: ~S" pos unrecognized))
            (parser-failure message)))))
    (count 0 0))

  (define (sep-lines str)
    (string-split str "\n" #t))

  (define (parse-instructions text)
    (foldr
      (lambda (line accumulator)
        (do-using <parser>
          (instructions <- accumulator)
          (instruction  <- (count-chicken line))
          (return (cons instruction instructions))))
      (<parser>-unit '())
      (sep-lines text)))

  ; ------------------------
  ; Utils
  ; ------------------------
  (define (clamp-number n lower upper)
    (min (max n lower) upper))

  ; ------------------------
  ; Instructions:
  ; ------------------------

  ; Instructions can be represented purely as integer numbers from 0 to infinity.
  ; Any number greater than 9 is considered a 'push' instruction, however.
  ; Thus, effectively, instruction opcodes can be clamped to a [0, 10] range.
  ;
  ; Note: In practice, the true value of a 'push' instruction affects the value pushed to the stack!
  ; The value to be pushed to the stack is calculated as: 
  ;   f(x) = x - 10
  ; Where x is the numeric of the push instruction.

  (define (to-opcode num)
    (max (inexact->exact num) 0))

  (define instruction-names
    #("axe"     ; exit program
      "chicken" ; push the string "chicken" onto the stack
      "add"     ; add two top stack values
      "fox"     ; subtract two top stack values
      "rooster" ; multiply two top stack values
      "compare" ; test equality of two top stack values, push result
      "pick"    ; double-wide, load value from stack or user input by index. too complex to sum up
      "peck"    ; store value from stack in another address. too complex to sum up.
      "fr"      ; jump instruction; self-explanatory. moves the instruction pointer.
      "BBQ"     ; pop x from stack, push &#x (tl;dr: html-escape char)
      "push"    ; pushes number to stack; already explained above
    ))

  (define (show-instruction instruction)
    (define opcode (to-opcode instruction))
    (cond
      ((<= opcode 9) (vector-ref instruction-names opcode))
      (else          (sprintf "push ~A" (- opcode 10)))))

  (define (show-instructions instructions)
    (define (show instructions car-is-operand)
      (if (null? instructions)
        '()
        (cons
          (if car-is-operand
            (sprintf "(operand: ~A)" (car instructions))
            (show-instruction (car instructions)))
          (show
            (cdr instructions) 
            (and (not car-is-operand) (= (car instructions) 6))))))
    (show instructions #f))
)

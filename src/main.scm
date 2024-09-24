(module chicken-in-chicken ()
  (import scheme (chicken base) (chicken string) (chicken format) srfi-13)

  (define chicken-string  "chicken")
  (define chicken-length  (string-length chicken-string))
  (define chicken-letters (string->list "chicken"))

  #|
  ------------------------
  Parser:
  ------------------------
  |#

  (define-record-type :parser
    (parser result value)
    parser? 
    (result parser-result)
    (value  parser-value))

  (define (parser-success value)
    (parser "success" value))

  (define (parser-failure value)
    (parser "failure" value))

  (define (parser-success? p)
    (and (parser? p) (string=? (parser-result p) "success")))

  (define (parser-failure? p)
    (and (parser? p) (string=? (parser-result p) "failure")))

  ; Monadic operations.
  ; The parser type is basically just an Either monad!

  ; pure
  (define parser-pure parser-success)

  ; bind (>>=)
  (define (parser-bind ma f)
    (if (parser-failure? ma) ma (f (parser-value ma))))

  ; then (>>)
  (define (parser-then ma mb)
    (parser-bind ma (lambda (_) mb)))

  #|
  ------------------------
  Parsing Chicken:
  ------------------------
  |#

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
        ((is-chicken line pos)
          (count (+ 1 num) (+ pos chicken-length)))
        ((is-space line pos)
          (count num (skip-spaces line pos)))
        ((>= pos len)
          (parser-success num))
        (else
          (let ()
            (define unrec (string-ref line pos))
            (define message (sprintf "unrecognized character at ~S: ~S" pos unrec))
            (parser-failure message)))))
    (count 0 0))

  (define (sep-lines str)
    (string-split str "\n" #t))

  (define (parse-instructions text)
    (foldr
      (lambda (line accumulator)
        (parser-bind accumulator
          (lambda (instructions)
            (parser-bind (count-chicken line)
              (lambda (instruction)
                (parser-pure (cons instruction instructions)))))))
      (parser-pure '()) ;; 0 lines is still a success!
      (sep-lines text)))
)

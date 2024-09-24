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
    (define lines (sep-lines text))
    (foldr
      (lambda (line acc)
        (define parsed-line (count-chicken line))
        (cond
          ((parser-failure? acc) acc)
          ((parser-failure? parsed-line) parsed-line)
          (else
            (let ((instructions (parser-value acc))
                  (instruction  (parser-value parsed-line)))
              (parser-success (cons instruction instructions))))))
      (parser-success '()) ;; 0 lines is still a success!
      lines))
)

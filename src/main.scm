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

  ;; Monad 'pure': 
  (define parser-pure parser-success)

  ;; Monad 'bind': (>>=)
  (define (parser-bind m f)
    (if (parser-success? m) (f (parser-value m)) m))

  ;; Monad 'then': (>>)
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
    (define lines (sep-lines text))
    (map
      parser-value
      (map count-chicken lines)))
)

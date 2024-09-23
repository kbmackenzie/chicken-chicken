(module chicken-in-chicken ()
  (import scheme (chicken base) srfi-13)

  (define chicken-string  "chicken")
  (define chicken-length  (string-length chicken-word))
  (define chicken-letters (string->list "chicken"))

  (define-record-type :parser
    (parser result value)
    parser? 
    (result parser-result)
    (value  parser-value))

  (define (count-chicken line) 
    (define (count num pos)
      (cond ((is-chicken line pos) (count (+ 1 num) (+ pos chicken-length)))
            ((is-whitespace line pos) (count (+ 1 num) (skip-whitespace line pos)))
            (else "error" #| todo: make this better |#)))
    (count 0 0))

  (define (sep-lines str)
    (#| todo |#))

  (define (str-take-while str predicate)
    (#| todo |#))
)

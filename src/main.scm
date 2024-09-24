(module chicken-in-chicken ()
  (import scheme (chicken base) srfi-13)

  (define chicken-string  "chicken")
  (define chicken-length  (string-length chicken-word))
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
    (string=? (parser-result p) "success"))

  (define (parser-failure? p)
    (string=? (parser-result p) "failure"))

  ;; Monad 'pure': 
  (define parser-pure parser-success)

  ;; Monad 'bind': (>>=)
  (define (parser-bind m f)
    (if (parser-success? m) (f (parser-value m)) m))

  ;; Monad 'then': (>>)
  (define (parser-then ma mb)
    (parser-bind ma (lambda (_) mb)))


  #| We can finally begin working on parsing instructions now! |#
  (define (count-chicken line) 
    (define len (string-length line))
    (define (count num pos)
      (cond
        ((is-chicken line pos)
          (count (+ 1 num) (+ pos chicken-length)))
        ((is-whitespace line pos)
          (count (+ 1 num) (skip-whitespace line pos)))
        ((>= pos len)
          (parser-success num))
        (else
          (begin
            (define unrec (string-ref line pos))
            (define message (sprintf "unrecognized character at ~S: ~S" pos unrec))
            (parser-failure message)))))
    (count 0 0))

  (define (sep-lines str)
    (#| todo |#))

  (define (str-take-while str predicate)
    (#| todo |#))
)

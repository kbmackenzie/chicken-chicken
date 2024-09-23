(import srfi-13)

(define chicken-word "chicken")
(define chicken-length (string-length chicken-word))

(define (string-drop-while str predicate)
  (define len (string-length str))
  (define (drop-rec i)
    (if (and (< i len) (predicate (string-ref str i)))
      (drop-rec (+ i 1))
      (string-drop str i)))
  (drop-rec 0))

(define (count-chicken line) (#| todo |#))

(define (sep-lines str) (#| todo |#))

(define (str-take-while str predicate) (#| todo |#))

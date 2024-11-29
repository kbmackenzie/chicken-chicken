(module (chicken-chicken utils) (is-left is-right with-either)
  (import scheme monad)

  (define (is-left e)
    (eqv? 'Left (car e)))

  (define (is-right e)
    (eqv? 'Right (car e)))

  (define (with-either on-left on-right e)
    (if (is-left e) (on-left (cdr e)) (on-right (cadr e))))
)

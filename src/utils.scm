(module (chicken-chicken utils) (concat-map enumerate-lines is-left is-right with-either)
  (import scheme (chicken base) srfi-1)

  (define (concat-map fn xs)
    (flatten (map fn xs)))

  (define (enumerate-lines lines)
    (zip (iota (length lines)) lines))

  (define (is-left e)
    (eqv? 'Left (car e)))

  (define (is-right e)
    (eqv? 'Right (car e)))

  (define (with-either on-left on-right e)
    (if (is-left e) (on-left (cadr e)) (on-right (cadr e))))
)

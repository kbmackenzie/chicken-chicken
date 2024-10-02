(module either-monad
  (either?
   either-tag
   either-value
   either-right
   either-right?
   either-left
   either-left?
   <either>
   <either>-unit
   <either>-bind
   <either>-fail)

  (define-record-type :either
    (either tag value)
    either? 
    (tag   either-tag)
    (value either-value))

  (define (either-right value)
    (either 'right value))

  (define (either-left value)
    (either 'left value))

  (define (either-right? e)
    (and (either? e) (eqv? (either-tag e) 'right)))

  (define (either-left? e)
    (and (either? e) (eqv? (either-tag e) 'left)))

  ; Monadic operations.
  (define-monad
    <either>
    either-right
    (lambda (m f)
      (if (either-left? m) m (f (either-value m))))
    either-left)
)

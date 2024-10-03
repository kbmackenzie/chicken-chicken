(module either-monad
  (either?
   either-tag
   either-value
   either-right
   either-right?
   either-left
   either-left?
   <either>-unit
   <either>-bind
   <either>-fail
   either-fmap
   either->string)

  (import scheme (chicken base) (chicken format) monad)

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
  (define-monad <either>
    either-right
    (lambda (m f)
      (if (either-left? m) m (f (either-value m))))
    either-left)

  (define (either-fmap f m)
    (do-using <either>
      (a <- m)
      (return (f a))))

  (define (either->string e)
    (sprintf "either(~A): ~S" (either-tag e) (either-value e)))
)

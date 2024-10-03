(import scheme (chicken base) (chicken format) (chicken io) (chicken process-context) srfi-1)
(import chicken-chicken either-monad)

(define (read-lines-enumerated path)
  (let* ((port     (open-input-file path))
         (contents (read-lines port)))
    (close-input-port port)
    (zip
      (iota (length contents))
      contents)))

(for-each
  (lambda (path)
    (define lines (read-lines-enumerated path))
    (define instructions
      (either-fmap
        (lambda (xs) (map instruction->string xs))
        (parse-instructions lines)))
    (print (either->string instructions)))
  (command-line-arguments))

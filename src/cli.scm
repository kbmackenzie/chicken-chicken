(import chicken-in-chicken)
(import scheme (chicken base) (chicken format) (chicken io) (chicken process-context))

(for-each
  (lambda (path)
    (define contents 
      (let* ((port     (open-input-file path))
             (contents (read-string #f port)))
        (close-input-port port)
        contents))
    (define instructions (parse-instructions contents))
    (define transform-value
      (if (parser-success? instructions)
        show-instructions
        (lambda (x) x)))
    (printf "parser ~A: ~S\n"
      (parser-result instructions)
      (transform-value (parser-value instructions))))
  (command-line-arguments))

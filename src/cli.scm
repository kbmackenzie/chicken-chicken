(import chicken-chicken)
(import scheme (chicken base) (chicken format) (chicken io) (chicken process-context) srfi-1)

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
    (define instructions (parse-instructions lines))
    (printf "parser ~A: ~S\n"
      (parser-result instructions)
      (if (parser-success? instructions)
        (map instruction->string (parser-value instructions))
        (parser-value instructions))))
  (command-line-arguments))

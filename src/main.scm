(import scheme (chicken base) (chicken format) (chicken io) (chicken process-context) srfi-1 monad)
(import (chicken-chicken compiler))

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
      (do/m <either>
        (>>=
          (parse-instructions lines)
          (lambda (xs) (return (map instruction->string xs))))))
    (printf "~S\n" instructions))
  (command-line-arguments))

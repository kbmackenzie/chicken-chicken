(import scheme (chicken format) (chicken io) (chicken process-context))
(import (chicken-chicken compiler) (chicken-chicken utils))

(define (read-lines-from-file path)
  (let* ((port     (open-input-file path))
         (contents (read-lines port)))
    (close-input-port port)
    contents))

(for-each
  (lambda (path)
    (with-either
      (lambda (err)    (fprint (current-error-port) "couldn't compile ~S: ~A" path err))
      (lambda (output) (print output))
      (compile (read-lines-from-file path))))
  (command-line-arguments))

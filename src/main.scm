(import scheme (chicken format) (chicken io) (chicken process-context))
(import (chicken-chicken compiler) (chicken-chicken utils))

(define (read-lines-from-file path)
  (let* ((port     (open-input-file path))
         (contents (read-lines port)))
    (close-input-port port)
    contents))

(define (compile-file path)
  (with-either
    (lambda (err)    (fprintf (current-error-port) "couldn't compile ~S: ~A\n" path err))
    (lambda (output) (print output))
    (compile (read-lines-from-file path))))

(define (inspect-file path)
  (with-either
    (lambda (err)    (fprintf (current-error-port) "couldn't parse ~S: ~A\n" path err))
    (lambda (output) (printf "~S\n" output))
    (inspect (read-lines-from-file path))))

(define (parse-args args)
  (cond
    ((string=? "inspect" (car args))  (inspect-file (cadr args)) )
    (else                             (compile-file (car args)))))

(print (command-line-arguments))
(parse-args (command-line-arguments))

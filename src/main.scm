(import scheme (chicken format) (chicken io) (chicken port) (chicken condition) (chicken process-context))
(import srfi-1 srfi-13 args)
(import (chicken-chicken compiler) (chicken-chicken utils))

; Parse command-line options.
(define cli-options
  (list
    (args:make-option (e esmodule) #:none              "Generate ESM-style export"     )
    (args:make-option (c commonjs) #:none              "Generate CommonJS-style export")
    (args:make-option (g global  ) #:none              "Generate global export"        )
    (args:make-option (o output  ) (#:required "PATH") "Output path"                   )
    (args:make-option (p compat  ) #:none              "Enable compatibility mode"     )
    (args:make-option (i inspect ) #:none              "Inspect instructions and leave")
    (args:make-option (h help    ) #:none              "Show help message"             )))

; Set of valid compiler mode symbols.
(define valid-modes '(esmodule commonjs global))

; Get an option symbol from the option map.
(define (get-mode options)
  (let*
    ((use-mode?
       (lambda (option)
         (and (memv (car option) valid-modes) (cdr option))))
     (accumulate
       (lambda (option mode)
         (if (use-mode? option) (car option) mode))))
    (fold accumulate 'none options)))

(define (get-compat options)
  (let ((compat (assv 'compat options)))
    (and compat (cdr compat))))

; -----------------------
; I/O: 
; -----------------------
(define (read-lines-from path)
  (let* ((is-stdin (string=? path "-"))
         (port     (if is-stdin (current-input-port) (open-input-file path)))
         (contents (read-lines port)))
    (if (not is-stdin) (close-input-port port))
    contents))

(define (compile-all paths options)
  (let*
    ((compiler-opts
       (compiler-options
         (get-mode options)
         (get-compat options)))

     (output-file
       (assv 'output options))

     (output-port
       (if output-file
         (open-output-file (cdr output-file))
         (current-output-port)))

     (compile-file
       (lambda (path)
         (with-either
           (lambda (err)    (fprintf (current-error-port) "couldn't compile ~S: ~A\n" path err))
           (lambda (output) (write-string output #f output-port) (newline output-port))
           (compile (read-lines-from path) compiler-opts))))

     (success? 
       (call-with-current-continuation
         (lambda (cont)
           (handle-exceptions
             exn
             (begin (print-error-message exn (current-error-port)) (cont #f))
             (for-each compile-file paths) #t)))))

    (if output-file (close-output-port output-port))
    (exit (if success? 0 1))))

(define (inspect-all paths)
  (let*
    ((inspect-file
       (lambda (path)
         (with-either
           (lambda (err)    (fprintf (current-error-port) "couldn't parse ~S: ~A\n" path err))
           (lambda (output) (print (string-join output "\n")))
           (inspect (read-lines-from path)))))
     (success?
       (call-with-current-continuation
         (lambda (cont)
           (handle-exceptions
             exn
             (begin (print-error-message exn (current-error-port)) (cont #f))
             (for-each inspect-file paths) #t)))))
    (exit (if success? 0 1))))

(define (show-help)
  (print "Usage: chicken-chicken [OPTIONS...] [FILES...]")
  (newline)
  (print* (args:usage cli-options)))

; -----------------------
; Parsing options:
; -----------------------
(define (parse-options)
  (receive (options operands) (args:parse (command-line-arguments) cli-options)
    (cond ((assv 'help options   ) (show-help))
          ((null? operands       ) (show-help) (exit 1))
          ((assv 'inspect options) (inspect-all operands))
          (else                    (compile-all operands options)))))
(parse-options)

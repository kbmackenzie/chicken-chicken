(import scheme (chicken format) (chicken io) (chicken port) (chicken file) (chicken process-context))
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

; -----------------------
; I/O: 
; -----------------------
(define (is-file? path)
  (and (file-exists? path) (not (directory-exists? path))))

(define (read-lines-from path)
  (let* ((is-stdin (string=? path "-"))
         (port     (if is-stdin (current-input-port) (open-input-file path)))
         (contents (read-lines port)))
    (if (not is-stdin) (close-input-port port))
    contents))

(define (compile-all paths options)
  (let
    ((compiler-opts
       (let ((compat (assv 'compat options)))
         (compiler-options
           (get-mode options)
           (and compat (cdr compat)))))

     (output-port
       (let ((output-file (assv 'output options)))
         (if output-file
           (open-output-file (cdr output-file))
           (current-output-port))))

     (is-stdout
       (not (assv 'output options))))

    ; compile all files:
    (with-output-to-port output-port
      (lambda ()
        (for-each
          (lambda (path)
            (with-either
              (lambda (err)    (fprintf (current-error-port) "couldn't compile ~S: ~A\n" path err))
              (lambda (output) (print output))
              (compile (read-lines-from path) compiler-opts)))
          paths)))
    (if (not is-stdout) (close-output-port output-port))))

(define (inspect-all paths)
  (for-each
    (lambda (path)
      (with-either
        (lambda (err)    (fprintf (current-error-port) "couldn't parse ~S: ~A\n" path err))
        (lambda (output) (print (string-join output "\n")))
        (inspect (read-lines-from path))))
    paths))

(define (show-help)
  (print "Usage: chicken-chicken [OPTIONS...] [FILES...]")
  (newline)
  (print* (args:usage cli-options)))

; -----------------------
; Parsing options:
; -----------------------
(define (parse-options)
  (receive (options operands) (args:parse (command-line-arguments) cli-options)
    (cond ((assv 'help    options) (show-help))
          ((assv 'inspect options) (inspect-all operands))
          (else                    (compile-all operands options)))))
(parse-options)

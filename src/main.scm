(import
  scheme
  (chicken format)
  (chicken io)
  (chicken port)
  (chicken file)
  (chicken process-context)
  srfi-1
  srfi-13
  monad
  (chicken-chicken compiler)
  (chicken-chicken utils)
  args)

; -----------------------
; Options:
; -----------------------

; Command-line options ~> grammar.
; Meant to be passed to 'args:parse'.
(define cli-options
  (list
    (args:make-option (e esmodule) #:none              "Generate ESM-style export"     )
    (args:make-option (c commonjs) #:none              "Generate CommonJS-style export")
    (args:make-option (g global  ) #:none              "Generate global export"        )
    (args:make-option (o output  ) (#:required "PATH") "Output path"                   )
    (args:make-option (p compat  ) #:none              "Enable compatibility mode"     )
    (args:make-option (i inspect ) #:none              "Inspect instructions and leave")
    (args:make-option (h help    ) #:none              "Show help message"             )))

; -----------------------
; Evaluating options:
; -----------------------

; Set of valid compiler mode symbols.
; The first argument to the compiler-options constructor.
(define valid-modes '(esmodule commonjs global))

; Determine mode to use from options.
; Folds over option alist in order.
(define (get-mode options)
  (let*
    ((use-mode?
       (lambda (option)
         (and (memv (car option) valid-modes) (cdr option))))
     (accumulate
       (lambda (option mode)
         (if (use-mode? option) (car option) mode))))
    (fold accumulate 'global options)))

; Determine whether to enable compatibility mode.
; The second arugment to the compiler-options constructor.
(define (get-compat options)
  (let ((compat (assv 'compat options)))
    (and compat (cdr compat))))

; Read all lines from a file (or from standard input).
; Note: It *WILL* throw an error if file doesn't exist. I handle it elsewhere.
(define (read-lines-from path)
  (do-using <either>
    (if (string=? path "-")
      (return (read-lines (current-input-port)))
      (cond
        ((not (file-exists? path)) (fail (sprintf "couldn't read file ~S" path)))
        ((directory-exists? path)  (fail (sprintf "expected file, got directory: ~S" path)))
        (else
          (let* ((port    (open-input-file path))
                 (content (read-lines port)     ))
            (close-input-port port)
            (return content)))))))

; Compile all input files.
; Evaluates options in assoc list in second argument.
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
       (lambda (path continuation)
         (do-using <either>
           (with-either
             (lambda (err)
               (fprintf (current-error-port) "error: couldn't compile ~S: ~A\n" path err)
               (continuation #f))
             (lambda (output)
               (write-string output #f output-port)
               (newline output-port))
             (>>=
               (read-lines-from path)
               (lambda (content) (compile content compiler-opts)))))))

     (success? 
       (call-with-current-continuation
         (lambda (continuation)
           (for-each (lambda (path) (compile-file path continuation)) paths)
           #t))))

    (if output-file (close-output-port output-port))
    (exit (if success? 0 1))))

; Inspect instructions in all input files.
; Always writes to standard output.
(define (inspect-all paths)
  (let*
    ((inspect-file
       (lambda (path continuation)
         (do-using <either>
           (with-either
             (lambda (err)
               (fprintf (current-error-port) "couldn't parse ~S: ~A\n" path err)
               (continuation #f))
             (lambda (output)
               (print (string-join output "\n")))
             (>>= (read-lines-from path) inspect)))))

     (success?
       (call-with-current-continuation
         (lambda (continuation)
           (for-each (lambda (path) (inspect-file path continuation)) paths)
           #t))))

    (exit (if success? 0 1))))

; Show --help message.
; Also shown when no input files are specified.
(define (show-help)
  (print "Usage: chicken-chicken [OPTIONS...] [FILES...]")
  (newline)
  (print* (args:usage cli-options)))

; -----------------------
; Running CLI: 
; -----------------------
(define (run)
  (receive (options operands) (args:parse (command-line-arguments) cli-options)
    (cond ((assv 'help options   ) (show-help))
          ((null? operands       ) (show-help) (exit 1))
          ((assv 'inspect options) (inspect-all operands))
          (else                    (compile-all operands options)))))
(run)

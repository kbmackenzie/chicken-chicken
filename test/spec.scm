(import scheme (chicken base) (chicken port) (chicken process) (chicken pathname) test)
(import (chicken-chicken compiler))

(define test-cases
  (list
    (('path "test/chicken/hello-world.chicken") ('expect "Hello world")                  )
    (('path "test/chicken/quine.chicken"      ) ('expect "chicken"    )                  )
    (('path "test/chicken/cat.chicken"        ) ('expect "foobar"     ) ('input "foobar"))))

(define (assoc-get key alist default)
  (let ((item (assoc key alist)))
    (if item (cdr item) default)))

(define (compile-file path)
  (call-with-input-pipe
    (string-append "./chicken-chicken " (qs path))
    (lambda (port) (read-string #f port))))

(define (generate-script path input)
  (string-append
    (compile-file path)
    (sprintf ";console.log(chicken(~S));" input)))

(define (run-chicken path input)
  (let ((script (generate-script path input)))
    (receive (stdout stdin pid) (process "node" '())
      (write-string script #f stdin)
      (let ((output (read-string #f stdout)))
        (close-input-port stdout)
        (close-output-port stdin)
        output))))

(define (run-test test-case)
  (let ((path   (assoc-get 'path   test-case ""))
        (input  (assoc-get 'input  test-case ""))
        (expect (assoc-get 'expect test-case "")))
    (test (pathname-file path) (run-chicken path input) expect)))

(define (run-all-tests)
  (test-group "compiling + running chicken scripts"
    (for-each run-test test-cases)))

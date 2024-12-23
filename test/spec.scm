(import
  scheme
  (chicken base)
  (chicken io)
  (chicken format)
  (chicken port)
  (chicken process)
  (chicken pathname)
  test
  srfi-13)

(define test-cases
  (list
    '((path "chicken/hello-world.chicken")                     (expect "Hello world"))
    '((path "chicken/quine.chicken"      )                     (expect "chicken"    ))
    '((path "chicken/cat.chicken"        ) (input "Cat input") (expect "Cat input"  ))
    '((path "chicken/deadfish.chicken"   ) (input "iissiso"  ) (expect "289"        ))))

(define (assoc-get key alist default)
  (let ((item (assoc key alist)))
    (if item (cadr item) default)))

(define (compile-file path)
  (call-with-input-pipe
    (string-append "./chicken-chicken -g " path)
    (lambda (port) (read-string #f port))))

(define (generate-script path input)
  (string-append
    (compile-file path)
    (sprintf "console.log(chicken(~S));" input)))

(define (run-chicken path input)
  (let ((script (generate-script path input)))
    (receive (stdout stdin _pid) (process "node" '())
      (write-string script #f stdin)
      (close-output-port stdin)
      (let ((output (read-string #f stdout)))
        (close-input-port stdout)
        (string-trim-both output)))))

(define (run-test test-case)
  (let ((path   (assoc-get 'path   test-case ""))
        (input  (assoc-get 'input  test-case ""))
        (expect (assoc-get 'expect test-case "")))
    (test-assert
      (pathname-file path)
      (string=? (run-chicken path input) expect))))

(define (run-all-tests)
  (test-group "compiling chicken scripts + running them with node"
    (for-each run-test test-cases)))
(run-all-tests)

(import scheme (chicken base) (chicken port) (chicken process) test)

(define test-cases
  (list
    (('path "chicken/hello-world.chicken") ('expect "Hello world")                  )
    (('path "chicken/quine.chicken"      ) ('expect "chicken"    )                  )
    (('path "chicken/cat.chicken"        ) ('expect "foobar"     ) ('input "foobar"))))

(define (assoc-get key alist default)
  (let ((item (assoc key alist)))
    (if item (cdr item) default)))

(define (generate-shell-line test-case)
  (sprintf
    "./test.sh ~A ~A ~A"
    (assoc-get 'path   test-case "")
    (assoc-get 'expect test-case "")
    (assoc-get 'input  test-case "")))

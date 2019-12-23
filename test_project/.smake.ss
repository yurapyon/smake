; smake default script

(define headers
  (list "wawa.h"))

(define sources
  (list "wawa.c"
        "blah.cpp"))

(define options "-W -Wall")

(define targets
  (flatten
    (list
      (map make-header-file headers)
      (make-basic-file "blah.cpp" '("wawa.h"))
      (make-basic-file "wawa.c" '("wawa.h"))
      (map
        (lambda (src)
          (make-c-object-file src options))
        sources)
      (make-executable "main" (map objize sources) "-W -Wall"))))

(define test
  (make-target "test"
               '("1" "2" "3")
               "echo $filename $sources $opt"
               "echo clean $filename"
               '(("opt" . "-W -Wall"))))

(define g (make-graph targets))

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

(define (build)
  (let ((tg (make-target-graph targets)))
    (target-graph-rebuild-changed tg)))

(define (clean)
  (for-each
    clean-target
    targets))

(define (install)
  (build-target
    (make-installed-file "install-dir/main" '("main"))))

; (set-default-command! build)

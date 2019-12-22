; #!~/usr/bin/env gsi

; smake
; [public domain]

(define (filter f lst)
  (fold-right
    (lambda (obj acc)
      (let ((t (f obj)))
        (if t
            (cons obj acc)
            acc)))
    '()
    lst))

;

(define (to-port p . args)
  (for-each
    (lambda (a)
      (display a p))
    args))

;

(define-type matrix
  constructor: _make-matrix
  read-only:
  _vec
  x-length
  y-length)

(define (make-matrix x-len y-len)
  (_make-matrix (make-vector (* x-len y-len))
                x-len
                y-len))

(define (matrix-index m x y)
  (+ (* (matrix-x-length m) x) y))

(define (matrix-ref m x y)
  (vector-ref (matrix-_vec m) (matrix-index m x y)))

(define (matrix-set! m x y to)
  (vector-set! (matrix-_vec m) (matrix-index m x y) to))

(define (matrix-map! f m)
  (let ((vec (matrix-_vec m)))
    (let rec ((n (- (vector-length vec) 1)))
      (when (< n 0)
        (vector-set! vec n
          (f (vector-ref vec n)))
        (rec (- n 1))))))

(define-type graph
  constructor: _make-graph
  read-only:
  matrix          ; x axis is start, y axis is end; for edge
  ordering
  lookup          ; target-name -> index
  reverse-lookup) ; index -> target

(define (make-graph targets)
  (let* ((node-ct (length targets))
         (_iota (iota node-ct))
         (matr (make-matrix node-ct node-ct))
         (lookup (make-table))
         (reverse-lookup (make-table)))
    (matrix-map!
      (lambda (_val) #f)
      matr)
    (map
      (lambda (tgt i)
        (table-set! lookup (target-filename tgt) i)
        (table-set! reverse-lookup i tgt))
      targets
      _iota)
    (map
      (lambda (tgt i)
        (let ((in-indicies
                (map
                  (lambda (src)
                    (or (table-ref lookup src #f)
                        (error "source not found" src)))
                  (target-sources tgt))))
          (map
            (lambda (idx)
              (matrix-set! matr idx i #t))
            in-indicies)))
      targets
      _iota)
    (_make-graph matr
                 (_gen-ordering matr)
                 lookup
                 reverse-lookup)))

(define (_gen-ordering matr)
  (letrec* ((node-ct (matrix-x-length matr))
            (_iota (iota node-ct))
            (mark-vec (make-vector node-ct #f))
            (push-vec (make-vector node-ct #f))
            (visit
              (lambda (idx)
                (if (vector-ref mark-vec idx)
                    (when (not (vector-ref push-vec idx))
                      (error "cycle detected"))
                    (let ((to-visit
                            (filter
                              (lambda (next)
                                ; todo could check for cycles here for better error reporting
                                (matrix-ref matr idx next))
                              _iota)))
                      (vector-set! mark-vec idx)
                      (map visit to-visit)
                      (vector-set! push-vec idx)
                      (set! ret (cons idx ret))))))
            (ret '()))
    (map visit _iota)
    ret))

(define (graph-object-ordering g)
  (let ((rl (graph-reverse-lookup g)))
    (map
      (lambda (idx)
        (table-ref rl idx))
      (graph-ordering g))))

; file system stuff

(define *script-filepath* (make-parameter ".smake.ss"))
(define *cache-filepath* (make-parameter "._smake.ss"))

(define default-script "; smake default script")
(define default-cache '((hashes . ())))

(define (configured?)
  (and (file-exists? (*script-filepath*))
       (file-exists? (*cache-filepath*))))

(define (assert-configured)
  (unless (configured?)
    (error "smake not configure. please call (configure)")))

(define (verbose-create-file type name init-fn)
  (unless (file-exists? name)
    (call-with-output-file name init-fn)
    (display type)
    (display " file ")
    (display name)
    (display " created")
    (newline)))

(define (verbose-delete-file type name)
  (when (file-exists? name)
    (delete-file name)
    (display type)
    (display " file ")
    (display name)
    (display " deleted")
    (newline)))

(define (configure)
  (unless (configured?)
    (verbose-create-file "script" (*script-filepath*)
      (lambda (p)
        (display default-script p)))
    (verbose-create-file "cache" (*cache-filepath*)
      (lambda (p)
        (write default-cache p)))
    (display "smake configured")
    (newline)))

(define (deconfigure #!optional delete-script)
  (verbose-delete-file "cache" (*cache-filepath*))
  (when delete-script
    (verbose-delete-file "script" (*script-filepath*))))

;

(define *initialized* (make-parameter #f))

(define (initialized)
  (assert-configured)
  (load-cache))
  ; load and run script

(define (assert-initialized)
  (unless (*initialized*)
    (error "smake not initialized")))

;

(define *cache* (make-parameter #f))
(define *hashes* (make-parameter #f))

(define (load-cache)
  (let ((alist (call-with-input-file (*cache-filepath*) read)))
    (*cache* (list->table alist))
    (*hashes* (list->table (cache-ref 'hashes)))))

(define (save-cache)
  (assert-initialized)
  (cache-set! 'hashes (table->list (*hashes*)))
  (call-with-output-file (*cache-filepath*)
    (lambda (p)
      (write (table->list (*cache*)) p))))

(define (cache-ref sym)
  (table-ref (*cache*) sym))

(define (cache-set! sym to)
  (table-set! (*cache*) sym to))

;

(define (gen-hash filename)
  (let* ((ret (shell-command (string-append "md5sum " filename) #t))
         (err (car ret))
         (hash (substring (cdr ret) 0 32)))
    (if (not (= err 0))
        (error "couldnt generate hash for file" filename)
        hash)))

; returns if hash matches cached hash
(define (check-hash filename)
  (assert-initialized)
  (let* ((ht (*hashes*))
         (cached (table-ref ht filename #f))))
  (if (not cached)
      #f
      (string=? cached (gen-hash filename))))

(define (update-hash! filename)
  (table-set! (*hashes*) filename (gen-hash filename)))

;

(define-type target
  read-only:
  filename
  sources     ; filenames
  rule        ; a fn to create target w/ filename
  clean-rule) ; usually #f

; rules are funcitons that take a target and return a commandline to run
; return "" to do nothing

;

(define *c-compiler* (make-parameter "gcc"))
(define *cpp-compiler* (make-parameter "g++"))

(define (objize filename)
  (let ((base (path-strip-extension filename)))
    (string-append
      (path-expand base "obj")
      ".o")))

(define (compiler-for-file filename)
  (let ((ftype (path-extension filename)))
    (cond
      ((string-ci=? ftype ".c") (*c-compiler*))
      ((or (string-ci=? ftype ".cpp")
           (string-ci=? ftype ".cc")) (*cpp-compiler*))
      (else
        (error "no compiler found:" filename)))))

; todo cleanup all this
; cmds can take a string or setenv variables
(define (make-system-cmd cmd)
  (lambda ()
    (let ((ret (shell-command cmd #t)))
      (when (not (= 0 (car ret)))
        (println (cdr ret))
        (error "command failed" cmd))
      (println (cdr ret)))))

(define (make-compile-str out in options)
  (let ((p (open-output-string)))
    (to-port p (compiler-for-file in) " ")
    (for-each
      (lambda (opt)
        (to-port p opt " "))
      options)
    (to-port p "-c " in " -o " out)
    (get-output-string p)))

(define (make-null-rule file)
  (lambda () #t))

(define (make-default-clean-rule file)
  (make-system-cmd (string-append "rm " file)))

(define (make-target-basic-file filename sources)
  (make-target filename
               sources
               (make-null-rule filename)
               (make-null-rule filename)))

(define (make-target-obj-file in options)
  (let ((obj-name (objize in)))
    (make-target obj-name
                 (list in)
                 (make-system-cmd (make-compile-str obj-name in options))
                 (make-default-clean-rule obj-name))))

(define targets
  (list
    (make-target "asdf.c" '() #f #f)
    (make-target "a.c" '("asdf.c" "b.c") #f #f)
    (make-target "b.c" '() #f #f)
    (make-target "d.c" '("a.c") #f #f)))

; (define (install-directory tgt)
  ; (rm deps)
  ; (cp (car deps) filename))

; smake [command] [options]

; (define (build ctx)
  ; (if (ctx-has-option? 'debug)
      ; (map
        ; (lambda (tgt)
          ; (add-target! ctx tgt)
        ; debug-targets)

; can cache the .smake file too
; check if files have changed and automatically rebuild

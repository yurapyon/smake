; #!~/usr/bin/env gsi

; smake

; can cache the .smake file too
; check if files have changed and automatically rebuild

(define (flatten lst)
  (let rec ((lst lst)
            (stack '())
            (acc '()))
    (cond
      ((null? lst)
       (if (null? stack)
           (reverse! acc)
           (rec (car stack)
                (cdr stack)
                acc)))
      ((not (pair? lst))
       (rec '()
            stack
            (cons lst acc)))
      (else
       (let ((obj (car lst)))
         (if (pair? obj)
             (if (null? (cdr lst))
                 (rec obj stack acc)
                 (rec obj
                      (cons (cdr lst) stack)
                      acc))
             (rec (cdr lst)
                  stack
                  (cons obj acc))))))))

(define (filter f lst)
  (fold-right
    (lambda (obj acc)
      (let ((t (f obj)))
        (if t
            (cons obj acc)
            acc)))
    '()
    lst))

; todo handle last separator
(define (join lst #!optional (separator " "))
  (call-with-output-string
    (lambda (p)
      (map
        (lambda (str)
          (display str p)
          (display separator p))
        lst))))

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
      (unless (< n 0)
        (vector-set! vec n
          (f (vector-ref vec n)))
        (rec (- n 1))))))

(define-type graph
  constructor: _make-graph
  read-only:
  matrix          ; x axis is start, y axis is end; for edge
  iota            ; here for memory usage purposes
  lookup          ; target-name -> index
  reverse-lookup) ; index -> target

(define (graph-node-count g)
  (matrix-x-length (graph-matrix g)))

; todo graph-node-count
;      clean up

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
                 _iota
                 lookup
                 reverse-lookup)))

(define (graph-index->target g idx)
  (let ((rl (graph-reverse-lookup g)))
    (table-ref rl idx)))

(define (graph-filename->index g fname)
  (let ((l (graph-lookup g)))
    (table-ref l fname)))

(define (graph-ordering g)
  (letrec* ((matr (graph-matrix g))
            (node-ct (graph-node-count g))
            (mark-vec (make-vector node-ct #f))
            (push-vec (make-vector node-ct #f))
            (ret '())
            (visit
              (lambda (idx)
                (if (vector-ref mark-vec idx)
                    (when (not (vector-ref push-vec idx))
                      (error "cycle detected"))
                    (begin
                      (vector-set! mark-vec idx #t)
                      (map visit (graph-get-children g idx))
                      (vector-set! push-vec idx #t)
                      (set! ret (cons idx ret)))))))
    (map visit (graph-iota g))
    ret))

(define (graph-get-children g idx)
  (let* ((matr (graph-matrix g)))
    (filter
      (lambda (node)
        (matrix-ref matr idx node))
      (graph-iota g))))

(define (graph-get-all-children g idx)
  (letrec* ((push-vec (make-vector (graph-node-count g) #f))
            (ret '())
            (visit
               (lambda (next)
                 (map visit (graph-get-children g next))
                 (when (not (vector-ref push-vec next))
                   (vector-set! push-vec next #t)
                   (set! ret (cons next ret))))))
    (visit idx)
    (cdr ret)))

(define (>> g val)
  (cond
    ((string? val)
     (graph-filename->index g val))
    ((target? val)
     (graph-filename->index g (target-filename val)))
    (else
     (error ">> error"))))

(define (% g val)
  (cond
    ((number? val)
     (graph-index->target g val))
    ((list? val)
     (map (lambda (v) (graph-index->target g v)) val))
    (else
     (error "g% error"))))

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
        (display default-script p)
        (newline p)))
    (verbose-create-file "cache" (*cache-filepath*)
      (lambda (p)
        (write default-cache p)
        (newline p)))
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

(define hash=? string=?)

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
         (cached (table-ref ht filename #f)))
    (and cached (hash=? cached (gen-hash filename)))))

(define (update-hash! filename)
  (table-set! (*hashes*) filename (gen-hash filename)))

;

; todo rename filename to output ?

(define-type target
  read-only:
  filename
  sources    ; filenames
  build-rule ; shell str to run when building
  clean-rule ; usually #f
  env-vars)  ; list of env vars to set when running rules

(define (with-target-environment tgt f)
  (setenv "filename" (target-filename tgt))
  (setenv "sources" (join (target-sources tgt)))
  (map
    (lambda (pair)
      (setenv (car pair) (cdr pair)))
    (target-env-vars tgt))
  (f)
  (map
    (lambda (pair)
      (setenv (car pair)))
    (target-env-vars tgt))
  (setenv "sources")
  (setenv "filename"))


; just returns err code from shell
(define (build-target tgt)
  (with-target-environment tgt
    (lambda ()
      (shell-command (target-build-rule tgt)))))

(define (clean-target tgt)
  (with-target-environment tgt
    (lambda ()
      (shell-command (target-clean-rule tgt)))))

;

(define (maybe-build targets)
  (let* ((g (make-graph targets))
         (changed-targets
           (filter
             (lambda (tgt)
               (not (check-hash (target-filename tgt))))
             targets)))
    0))

;

(define *c-compiler* (make-parameter "gcc"))
(define *cpp-compiler* (make-parameter "g++"))
(define *exe-linker* (make-parameter "g++"))
(define *object-directory* (make-parameter "obj"))

(define (objize filename)
  (let ((base (path-strip-extension filename)))
    (string-append
      (path-expand base (*object-directory*))
      ".o")))

(define (compiler-for-file filename)
  (let ((ftype (path-extension filename)))
    (cond
      ((string-ci=? ftype ".c") (*c-compiler*))
      ((or (string-ci=? ftype ".cpp")
           (string-ci=? ftype ".cc")) (*cpp-compiler*))
      (else
        (error "no compiler found:" filename)))))

(define (make-basic-file filename sources)
  (make-target filename
               sources
               ""
               ""
              '()))

(define (make-header-file filename)
  (make-basic-file filename '()))

(define (make-c-object-file in options)
  (let ((obj-name (objize in)))
    (make-target obj-name
                 (list in)
                 "$compiler $options -c $sources -o $filename"
                 "rm -f $filename"
                 `(("compiler" . ,(compiler-for-file in))
                   ("options" . ,options)))))

(define (make-executable out sources options)
    (make-target out
                 sources
                 "$linker $options $sources -o $filename"
                 "rm -f $filename"
                 `(("linker" . ,(*exe-linker*))
                   ("options" . ,options))))

(define (make-installed-file dest src)
  (make-target dest
               src
               "rm -f $filename; cp $sources $filename"
               "rm -f $filename"))

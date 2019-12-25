; todo
;   can cache the .smake.ss file too
;   think about how to stop rebuild
;     ie adding a comment to a file
;   clean should reset cache?

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

(define (always val)
  (lambda args val))

(define (to-port p . args)
  (for-each
    (lambda (a)
      (display a p))
    args))

;

(define-type range
  constructor: _make-range
  read-only:
  start
  end
  step)

(define (make-range start end #!optional (step 1))
  (_make-range start end step))

(define (range-for-each f range)
  (let ((end (range-end range))
        (step (range-step range)))
     (let rec ((n (range-start range)))
        (when (< n end)
          (f n)
          (rec (+ n step))))))

(define (range-_pmap pred f range)
  (let ((ret '())
        (end (range-end range))
        (step (range-step range)))
     (let rec ((n (range-start range)))
        (if (>= n end)
            (reverse! ret)
            (begin
              (when (pred n)
                (set! ret (cons (f n) ret)))
              (rec (+ n step)))))))

(define (range-map f range)
  (range-_pmap (always #t) f range))

(define (range-filter pred range)
  (range-_pmap pred identity range))

;

(define-type matrix
  constructor: _make-matrix
  read-only:
  _vec
  x-length
  y-length)

(define (make-matrix x-len y-len . init-args)
  (_make-matrix (apply make-vector (* x-len y-len) init-args)
                x-len
                y-len))

(define (matrix-_vec-index m x y)
  (+ (* (matrix-x-length m) x) y))

(define (matrix-ref m x y)
  (vector-ref (matrix-_vec m) (matrix-_vec-index m x y)))

(define (matrix-set! m x y to)
  (vector-set! (matrix-_vec m) (matrix-_vec-index m x y) to))

;

(define-type graph
  constructor: _make-graph
  read-only:
  matrix     ; x axis is start, y axis is end; for edge
  lookup     ; obj-id -> index
  objects    ; vector
  range)

(define (make-graph obj-lst id-fn sources-fn)
  (let* ((node-ct (length obj-lst))
         (matr (make-matrix node-ct node-ct #f))
         (lookup (make-table))
         (objects (list->vector obj-lst))
         (range (make-range 0 node-ct)))
    (range-for-each
      (lambda (i)
        (let ((obj (vector-ref objects i)))
          (table-set! lookup (id-fn obj) i)))
      range)
    (range-for-each
      (lambda (i)
        (let* ((obj (vector-ref objects i))
               (src-indicies
                 (map
                   (lambda (src)
                     (or (table-ref lookup src #f)
                         (error "source not found" src)))
                   (sources-fn obj))))
          (map
            (lambda (src-idx)
              (matrix-set! matr src-idx i #t))
            src-indicies)))
      range)
    (let ((g (_make-graph matr
                          lookup
                          objects
                          range)))
      ; order the graph to detect cycles
      (graph-ordering g)
      g)))

(define (graph-node-count g)
  (matrix-x-length (graph-matrix g)))

(define (graph-index->object g idx)
  (vector-ref (graph-objects g) idx))

(define (graph-object-id->index g id)
  (table-ref (graph-lookup g) id))

(define (graph-get-children g idx)
  (let ((matr (graph-matrix g)))
    (range-filter
      (lambda (i)
        (matrix-ref matr idx i))
      (graph-range g))))

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
                      ; todo better error reporting use obj-ids
                      (error "cycle detected"))
                    (begin
                      (vector-set! mark-vec idx #t)
                      (map visit (graph-get-children g idx))
                      (vector-set! push-vec idx #t)
                      (set! ret (cons idx ret)))))))
    (range-map visit (graph-range g))
    ret))

(define (graph-propagate p g)
  (letrec ((p-vec (make-vector (graph-node-count g) #f))
           (mark!
             (lambda (idx)
               (unless (vector-ref p-vec idx)
                 (vector-set! p-vec idx #t)
                 (for-each
                   mark!
                   (graph-get-all-children g idx))))))
    (range-for-each
      (lambda (i)
        (let ((obj (graph-index->object g i)))
          (when (p obj)
            (mark! i))))
      (graph-range g))
    (filter
      (lambda (i)
        (vector-ref p-vec i))
      (graph-ordering g))))

; file system stuff

; todo make these configureable?
;      if not, just use constants not parameters
(define *script-filepath* (make-parameter ".smake.ss"))
(define *cache-filepath* (make-parameter "._smake.ss"))

(define default-script "; smake default script")
(define default-cache '((hashes . ())))

(define (configured?)
  (and (file-exists? (*script-filepath*))
       (file-exists? (*cache-filepath*))))

(define (assert-configured)
  (unless (configured?)
    (error "smake not configured. please call (configure)")))

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

(define (initialize)
  (assert-configured)
  (load-cache)
  (load (*script-filepath*))
  (*initialized* #t))

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
  (if (not (file-exists? filename))
      #f
      (let* ((ht (*hashes*))
             (cached (table-ref ht filename #f)))
        (and cached (hash=? cached (gen-hash filename))))))

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

(define (make-target-graph targets)
  (make-graph targets target-filename target-sources))

(define (target-graph-get-changed g)
  (map
    (lambda (idx)
      (graph-index->object g idx))
    (graph-propagate
      (lambda (tgt)
        (not (check-hash (target-filename tgt))))
      g)))

(define (target-graph-rebuild-changed g)
  (let ((changed (target-graph-get-changed g)))
    (for-each
      (lambda (tgt)
        ; todo handle if failed
        (display "building ")
        (display (target-filename tgt))
        (newline)
        (build-target tgt)
        (update-hash! (target-filename tgt)))
      changed)
    (save-cache)))

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
               "rm -f $filename"
               '()))

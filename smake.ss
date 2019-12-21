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
            (is-marked?
              (lambda (idx)
                (vector-ref mark-vec idx)))
            (mark!
              (lambda (idx)
                (vector-set! mark-vec idx #t)))
            (push-vec (make-vector node-ct #f))
            (is-pushed?
              (lambda (idx)
                (vector-ref push-vec idx)))
            (push!
              (lambda (idx)
                (vector-set! push-vec idx #t)))
            (visit
              (lambda (idx)
                (if (is-marked? idx)
                    (when (not (is-pushed? idx))
                      (error "cycle detected"))
                    (let ((to-visit
                            (filter
                              (lambda (next)
                                ; todo could check for cycles here for better error reporting
                                (matrix-ref matr idx next))
                              _iota)))
                      (mark! idx)
                      (map visit to-visit)
                      (push! idx)
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

(define-type target
  read-only:
  filename
  sources     ; filenames
  rule        ; a fn to create target w/ filename
  clean-rule) ; usually #f

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

; automatic style
;   dependency graph
;     check if files have been changed
;     keep a cache
;   actions are added on the command line
;   cant do things file by file

; manual style
;   command line completion
;     focus on 'targets' to be made
;   batch files to 'do'
;   separate
;     compile
;     link
;     install
;   infered
;     compile & link
;   action group
;     ex. release
;         debug
;         test
;         install > (do make stuff)
;                   (do install stuff)
;         clean > (undo make stuff)
;                 (undo installed stuff)
;   clean/uninstall

; default target-set
; default action

; config what smake with no args does

; use default target set
; smake main.cpp main

; do default action for tset install
; smake -t install

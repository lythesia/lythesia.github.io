title: Notes for TYSS
date: 2017-06-15 19:03:16
categories: lang
tags: [scheme, note]
---

baka notes for [teach-yourself-scheme-in-fixnum-days](http://ds26gte.github.io/tyscheme/)

Q: why abbr as TYSS not TYSIFD?
A: i like it :P

this (these) notes may not contain all elements of scheme (e.g. define-syntax&syntax-rules), and some chaps are skipped.

i just keep what i've got.

----

# Ch2

## type

literal cons pair: `'(1 . 2)`, we need quote to avoid being evaluated as proc apply

## concept

Port:

> Ports are usually associated with files and consoles.

`(format (current-output-port) "im format string" args...)`

----

# Ch8 

## macro

looks like

```scheme
(define-macro (..)
  ; usually
  `(..
    ,(eval-to-single-var)
    ,@(eval-to-list)
  )
  ; so use it as **template**!
)
```

one class/object maker example illustrated:

```scheme
(define ufo (if #f #f))

;; (defstruct name . field-decl)
;; : field-decl := f-name | (f-name default-val)
;; => (make-$name [f-name f-val])
;; => ($name.$f-name obj)

(define-macro (defstruct st-name . st-fields)
  (let* (
    ;; fields-related
    (symbol-of (lambda (x) (if (pair? x) (car x) x)))
    (value-of (lambda (x) (if (pair? x) (cadr x) ufo)))
    (st-field-syms (map symbol-of st-fields))
    (st-field-count (length st-fields))
    (st-field-getter (lambda (f-name) (symbol-append st-name '. f-name)))
    (st-field-setter (lambda (f-name) (symbol-append 'set! st-name '. f-name)))
    ;; struct-name-related
    (st-maker (symbol-append 'make- st-name))
    (st-identifier (symbol-append st-name '?))
    ;; storage
    (st-size (1+ st-field-count))
    (st-fields-default-vals (map value-of st-fields))
    )
    `(begin
      ;; maker
      (define (,st-maker . field-val-list)
         (let (
           ;; holder (init with header + defaults)
           (data (list->vector (cons ',st-name ',st-fields-default-vals)))
           )
           ;; override if arg provided
           (let loop ((rest-pairs field-val-list))
             (if (not (null? rest-pairs))
               (begin
                 (let (
                       (arg-name (car rest-pairs))
                       (arg-val (cadr rest-pairs))
                       (list-find-index
                         (lambda (x lst)
                           (begin
                             (define (iter i rest)
                               (cond
                                 ((null? rest) ufo)
                                 ((eq? (car rest) x) i)
                                 (else (iter (1+ i) (cdr rest)))
                               )
                             )
                             (iter 0 lst)
                           )
                         )
                       ))
                   (vector-set! data (1+ (list-find-index arg-name ',st-field-syms)) arg-val)
                   (loop (cddr rest-pairs))
                 )
               )
             )
           )
           ;; return vector
           data
         )
      )
      ;; identifier
      (define (,st-identifier st-obj)
        (and
         (vector? st-obj)
         (eq? ,st-name (vector-ref st-obj 0))
        )
      )
      ;; getter&setter
      ,@(let loop ((i 0) (procs '()))
          (if (< i st-field-count)
            (loop (1+ i)
                  (let ((f-name (list-ref st-field-syms i)))
                    (cons
                      `(define ,(st-field-getter f-name) (lambda (st-obj) (vector-ref st-obj ,(1+ i))))
                      (cons
                        `(define ,(st-field-setter f-name) (lambda (st-obj val) (vector-set! st-obj ,(1+ i) val)))
                        procs
                      )
                    )
                  )
            )
            procs
          )
      )
    )
  )
)
```

points:

* storage as tagged vector
* init with default values (ufo if not provided)
* override with real values

----

# Ch10

## alist

sth like `((a . 1) (b . 2))`

`(assv key alist)` find *cons cell* in `alist` with `key`

----

つづく

; File: "picobit.scm", Time-stamp: <2006-05-08 16:04:37 feeley>

; Copyright (C) 2008 by Marc Feeley and Vincent St-Amour, All Rights Reserved.

(define-macro (dummy)
  (proper-tail-calls-set! #f)
  #f)
;(dummy)

;-----------------------------------------------------------------------------

(define compiler-error
  (lambda (msg . others)
    (display "*** ERROR -- ")
    (display msg)
    (for-each (lambda (x) (display " ") (write x)) others)
    (newline)
    (exit 1)))

;-----------------------------------------------------------------------------

(define keep
  (lambda (keep? lst)
    (cond ((null? lst)       '())
          ((keep? (car lst)) (cons (car lst) (keep keep? (cdr lst))))
          (else              (keep keep? (cdr lst))))))

(define take
  (lambda (n lst)
    (if (> n 0)
        (cons (car lst) (take (- n 1) (cdr lst)))
        '())))

(define drop
  (lambda (n lst)
    (if (> n 0)
        (drop (- n 1) (cdr lst))
        lst)))

(define repeat
  (lambda (n x)
    (if (> n 0)
        (cons x (repeat (- n 1) x))
        '())))

(define pos-in-list
  (lambda (x lst)
    (let loop ((lst lst) (i 0))
      (cond ((not (pair? lst)) #f)
            ((eq? (car lst) x) i)
            (else              (loop (cdr lst) (+ i 1)))))))

(define every
  (lambda (pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (every pred? (cdr lst))))))

;-----------------------------------------------------------------------------

;; Syntax-tree node representation.

(define-type node
  extender: define-type-of-node
  (parent unprintable:)
  children
)

(define-type-of-node cst
  val
)

(define-type-of-node ref
  var
)

(define-type-of-node def
  var
)

(define-type-of-node set
  var
)

(define-type-of-node if
)

(define-type-of-node prc
  params
  rest?
  entry-label
)

(define-type-of-node call
)

(define-type-of-node seq
)

(define-type-of-node fix
  vars
)

(define node->expr
  (lambda (node)
    (cond ((cst? node)
           (let ((val (cst-val node)))
             (if (self-eval? val)
                 val
                 (list 'quote val))))
          ((ref? node)
           (var-id (ref-var node)))
          ((def? node)
           (list 'define
                 (var-id (def-var node))
                 (node->expr (child1 node))))
          ((set? node)
           (list 'set!
                 (var-id (set-var node))
                 (node->expr (child1 node))))
          ((if? node)
           (list 'if
                 (node->expr (child1 node))
                 (node->expr (child2 node))
                 (node->expr (child3 node))))
          ((prc? node)
           (if (seq? (child1 node))
               (cons 'lambda
                     (cons (build-pattern (prc-params node) (prc-rest? node))
                           (nodes->exprs (node-children (child1 node)))))
               (list 'lambda
                     (build-pattern (prc-params node) (prc-rest? node))
                     (node->expr (child1 node)))))
          ((call? node)
           (map node->expr (node-children node)))
          ((seq? node)
           (let ((children (node-children node)))
             (cond ((null? children)
                    '(void))
                   ((null? (cdr children))
                    (node->expr (car children)))
                   (else
                    (cons 'begin
                          (nodes->exprs children))))))
          ((fix? node)
           (let ((children (node-children node)))
             (list 'letrec
                   (map (lambda (var val)
                          (list (var-id var)
                                (node->expr val)))
                        (fix-vars node)
                        (take (- (length children) 1) children))
                   (node->expr (list-ref children (- (length children) 1))))))
          (else
           (compiler-error "unknown expression type" node)))))

(define nodes->exprs
  (lambda (nodes)
    (if (null? nodes)
        '()
        (if (seq? (car nodes))
            (append (nodes->exprs (node-children (car nodes)))
                    (nodes->exprs (cdr nodes)))
            (cons (node->expr (car nodes))
                  (nodes->exprs (cdr nodes)))))))
            
(define build-pattern
  (lambda (params rest?)
    (cond ((null? params)
           '())
          ((null? (cdr params))
           (if rest?
               (var-id (car params))
               (list (var-id (car params)))))
          (else
           (cons (var-id (car params))
                 (build-pattern (cdr params) rest?))))))

;-----------------------------------------------------------------------------

;; Environment representation.

(define-type var
  id
  global?
  (refs unprintable:) 
  (sets unprintable:)
  (defs unprintable:)
  needed?
  primitive
)

(define-type primitive
  nargs
  inliner
  unspecified-result?
)

(define-type renaming
  renamings
)

(define make-global-env
  (lambda ()
    (list
     (make-var '#%number? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%+ #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%- #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%* #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%quotient #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%remainder #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%neg #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%= #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%< #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%ior #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%> #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%xor #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%pair? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%cons #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%car #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%cdr #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%set-car! #t '() '() '() #f (make-primitive 2 #f #t))
     (make-var '#%set-cdr! #t '() '() '() #f (make-primitive 2 #f #t))
     (make-var '#%null? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%eq? #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%not #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%get-cont #t '() '() '() #f (make-primitive 0 #f #f))
     (make-var '#%graft-to-cont #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%return-to-cont #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%halt #t '() '() '() #f (make-primitive 0 #f #t))
     (make-var '#%symbol? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%string? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%string->list #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%list->string #t '() '() '() #f (make-primitive 1 #f #f))     
     (make-var '#%make-u8vector #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%u8vector-ref #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%u8vector-set! #t '() '() '() #f (make-primitive 3 #f #t))
     (make-var '#%print #t '() '() '() #f (make-primitive 1 #f #t))
     (make-var '#%clock #t '() '() '() #f (make-primitive 0 #f #f))
     (make-var '#%motor #t '() '() '() #f (make-primitive 2 #f #t))
     (make-var '#%led #t '() '() '() #f (make-primitive 3 #f #t))
     (make-var '#%led2-color #t '() '() '() #f (make-primitive 1 #f #t))
     (make-var '#%getchar-wait #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%putchar #t '() '() '() #f (make-primitive 2 #f #t))
     (make-var '#%beep #t '() '() '() #f (make-primitive 2 #f #f))
     (make-var '#%adc #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%u8vector? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%sernum #t '() '() '() #f (make-primitive 0 #f #f))
     (make-var '#%u8vector-length #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%u8vector-copy! #t '() '() '() #f (make-primitive 5 #f #t))
     (make-var '#%boolean? #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%network-init #t '() '() '() #f (make-primitive 0 #f #t))
     (make-var '#%network-cleanup #t '() '() '() #f (make-primitive 0 #f #t))
     (make-var '#%receive-packet-to-u8vector #t '() '() '() #f (make-primitive 1 #f #f))
     (make-var '#%send-packet-from-u8vector #t '() '() '() #f (make-primitive 2 #f #f))
     
     (make-var '#%readyq #t '() '() '() #f #f)
     ;; TODO put in a meaningful order
     )))

;; list of primitives that can be safely substituted for the equivalent
;; function when it is called.
;; this saves the calls to the primitive wrapper functions, which are still
;; needed if a program needs the value of a "primitive", for example in :
;; (define foo car)
(define substitute-primitives
  '((number? . #%number?)
    (quotient . #%quotient)
    (remainder . #%remainder)
    (= . #%=)
    (< . #%<)
    (> . #%>)
    (pair? . #%pair?)
    (cons . #%cons)
    (car . #%car)
    (cdr . #%cdr)
    (set-car! . #%set-car!)
    (set-cdr! . #%set-cdr!)
    (null? . #%null?)
    (eq? . #%eq?)
    (not . #%not)
    (modulo . #%remainder)
    (symbol? . #%symbol?)
    (string? . #%string?)
    (string->list . #%string->list)
    (list->string . #%list->string)
    (clock . #%clock)
    (beep . #%beep)
    (light . #%adc)
    (adc . #%adc)
    (sernum . #%sernum)
    (motor . #%motor)
    (led . #%led)
    (bitwise-ior . #%ior)
    (bitwise-xor . #%xor)
    (current-time . #%clock)
    (u8vector-length . #%u8vector-length)
    (u8vector-ref . #%u8vector-ref)
    (u8vector-set! . #%u8vector-set!)
    (make-u8vector . #%make-u8vector)
    (u8vector-copy! . #%u8vector-copy!)
    (boolean? . #%boolean?)
    (network-init . #%network-init)
    (network-cleanup . #%network-cleanup)
    (receive-packet-to-u8vector . #%receive-packet-to-u8vector)
    (send-packet-from-u8vector . #%send-packet-from-u8vector)
    ))

(define env-lookup
  (lambda (env id)
    (let loop ((lst env) (id id))
      (let ((b (car lst)))
        (cond ((and (renaming? b)
                    (assq id (renaming-renamings b)))
               =>
               (lambda (x)
                 (loop (cdr lst) (cadr x))))
              ((and (var? b)
                    (eq? (var-id b) id))
               b)
              ((null? (cdr lst))
               (let ((x (make-var id #t '() '() '() #f #f)))
                 (set-cdr! lst (cons x '()))
                 x))
              (else
               (loop (cdr lst) id)))))))

(define env-extend
  (lambda (env ids def)
    (append (map (lambda (id)
                   (make-var id #f '() '() (list def) #f #f))
                 ids)
            env)))

(define env-extend-renamings
  (lambda (env renamings)
    (cons (make-renaming renamings) env)))

(define *macros* '())

;-----------------------------------------------------------------------------

;; Parsing.

(define parse-program
  (lambda (expr env)
    (let ((x (parse-top expr env)))
      (cond ((null? x)
             (parse 'value #f env))
            ((null? (cdr x))
             (car x))
            (else
             (let ((r (make-seq #f x)))
               (for-each (lambda (y) (node-parent-set! y r)) x)
               r))))))

(define parse-top
  (lambda (expr env)
    (cond ((and (pair? expr)
                (eq? (car expr) 'define-macro))
           (set! *macros*
		 (cons (cons (caadr expr)
			     (eval `(lambda ,(cdadr expr) . ,(cddr expr))))
		       *macros*))
           '())
	  ((and (pair? expr)
		(eq? (car expr) 'begin))
           (parse-top-list (cdr expr) env))
          ((and (pair? expr)
                (eq? (car expr) 'hide))
           (parse-top-hide (cadr expr)  (cddr expr) env))
          ((and (pair? expr)
                (eq? (car expr) 'rename))
           (parse-top-rename (cadr expr)  (cddr expr) env))
          ((and (pair? expr)
                (eq? (car expr) 'define))
           (let ((var
                  (if (pair? (cadr expr))
                      (car (cadr expr))
                      (cadr expr)))
                 (val
                  (if (pair? (cadr expr))
                      (cons 'lambda (cons (cdr (cadr expr)) (cddr expr)))
                      (caddr expr))))
             (let* ((var2 (env-lookup env var))
                    (val2 (parse 'value val env))
                    (r (make-def #f (list val2) var2)))
               (node-parent-set! val2 r)
               (var-defs-set! var2 (cons r (var-defs var2)))
               (list r))))
          (else
           (list (parse 'value expr env))))))

(define parse-top-list
  (lambda (lst env)
    (if (pair? lst)
        (append (parse-top (car lst) env)
                (parse-top-list (cdr lst) env))
        '())))

(define parse-top-hide
  (lambda (renamings body env)
    (append
     (parse-top-list body
                     (env-extend-renamings env renamings))
     ;; (parse-top-list
     ;;       (map (lambda (x) (list 'define (car x) (cadr x))) renamings)
     ;;       env)
     )))

(define parse-top-rename
  (lambda (renamings body env)
    (parse-top-list body
                    (env-extend-renamings env renamings))))

(define parse
  (lambda (use expr env)
    (cond ((self-eval? expr)
           (make-cst #f '() expr))
          ((symbol? expr)
	   (let* ((var (env-lookup env expr))
		  (r (make-ref #f '() var)))
	     (var-refs-set! var (cons r (var-refs var)))
	     (if (not (var-global? var))
		 (let* ((unbox (parse 'value '#%unbox env))
			(app (make-call #f (list unbox r))))
		   (node-parent-set! r app)
		   (node-parent-set! unbox app)
		   app)
		 r)))
	  ((and (pair? expr)
                (assq (car expr) *macros*))
           => (lambda (p) (parse use (apply (cdr p) (cdr expr)) env)))
          ((and (pair? expr)
                (eq? (car expr) 'set!))
           (let ((var (env-lookup env (cadr expr))))
             (if (var-global? var)
                 (let* ((val (parse 'value (caddr expr) env))
                        (r (make-set #f (list val) var)))
                   (node-parent-set! val r)
                   (var-sets-set! var (cons r (var-sets var)))
                   r)
		 (let* ((body (parse 'value (caddr expr) env))
                        (ref (make-ref #f '() var))
                        (bs (make-ref #f '() (env-lookup env '#%box-set!)))
                        (r (make-call #f (list bs ref body))))
                   (node-parent-set! body r)
                   (node-parent-set! ref r)
                   (node-parent-set! bs r)
                   (var-sets-set! var (cons r (var-sets var)))
                   r))))
          ((and (pair? expr)
                (eq? (car expr) 'quote))
           (make-cst #f '() (cadr expr)))
          ((and (pair? expr)
                (eq? (car expr) 'if))
           (let* ((a (parse 'test (cadr expr) env))
                  (b (parse use (caddr expr) env))
                  (c (if (null? (cdddr expr))
                         (make-cst #f '() #f)
                         (parse use (cadddr expr) env)))
                  (r (make-if #f (list a b c))))
             (node-parent-set! a r)
             (node-parent-set! b r)
             (node-parent-set! c r)
             r))
          ((and (pair? expr)
                (eq? (car expr) 'lambda))
	   (let* ((pattern (cadr expr))
                  (ids (extract-ids pattern))
		  ;; parent children params rest? entry-label
                  (r (make-prc #f '() #f (has-rest-param? pattern) #f))
                  (new-env (env-extend env ids r))
                  (body (parse-body (cddr expr) new-env))
                  (mut-vars
		   (apply append
			  (map (lambda (id)
				 (let ((v (env-lookup new-env id)))
				   (if (mutable-var? v) (list v) '())))
			       ids))))
             (if (null? mut-vars)
                 (begin
                   (prc-params-set! r
				    (map (lambda (id) (env-lookup new-env id))
					 ids))
                   (node-children-set! r (list body))
                   (node-parent-set! body r)
                   r)
                 (let* ((prc (make-prc #f (list body) mut-vars #f #f))
                        (new-vars (map var-id mut-vars))
                        (tmp-env (env-extend env new-vars r))
                        (app
			 (make-call
			  r
			  (cons prc
				(map (lambda (id)
				       (parse 'value
					      (cons '#%box (cons id '()))
					      tmp-env))
				     new-vars)))))
                   ;; (lambda (a b) (set! a b))
		   ;; => (lambda (_a b) ((lambda (a) (box-set! a b)) (box _a)))
                   (for-each (lambda (var) (var-defs-set! var (list prc)))
                             mut-vars)
                   (for-each (lambda (n) (node-parent-set! n app))
			     (cdr (node-children app)))
                   (node-parent-set! prc app)
                   (prc-params-set! r
				    (map (lambda (id) (env-lookup tmp-env id))
					 ids))
                   (node-children-set! r (list app))
                   (node-parent-set! body prc)
                   r))))
	  ((and (pair? expr)
                (eq? (car expr) 'letrec))
           (let ((ks (map car (cadr expr)))
                 (vs (map cadr (cadr expr))))
             (parse use
                    (cons 'let
			  (cons (map (lambda (k) (list k #f)) ks)
				(append (map (lambda (k v) (list 'set! k v))
					     ks vs) ; letrec*
					(cddr expr))))
                    env)))
          ((and (pair? expr)
                (eq? (car expr) 'begin))
           (let* ((exprs (map (lambda (x) (parse 'value x env)) (cdr expr)))
                  (r (make-seq #f exprs)))
             (for-each (lambda (x) (node-parent-set! x r)) exprs)
             r))
          ((and (pair? expr)
                (eq? (car expr) 'let))
           (if (symbol? (cadr expr))
	       (parse use
                      `(letrec ((,(cadr expr) (lambda ,(map car (caddr expr)) .
                                                      ,(cdddr expr))))
                         (,(cadr expr) . ,(map cadr (caddr expr))))
                      env)
               (parse use
                      (cons (cons 'lambda
                                  (cons (map car (cadr expr))
                                        (cddr expr)))
                            (map cadr (cadr expr)))
                      env)))
          ((and (pair? expr)
                (eq? (car expr) 'let*))
           (if (null? (cadr expr))
               (parse use
                      (cons 'let (cdr expr))
                      env)
               (parse use
                      (list 'let
                            (list (list (caar (cadr expr))
                                        (cadar (cadr expr))))
                            (cons 'let*
                                  (cons (cdr (cadr expr))
                                        (cddr expr))))
                      env)))
          ((and (pair? expr)
                (eq? (car expr) 'and))
           (cond ((null? (cdr expr))
                  (parse use
                         #t
                         env))
                 ((null? (cddr expr))
                  (parse use
                         (cadr expr)
                         env))
                 (else
                  (parse use
                         (list 'if
                               (cadr expr)
                               (cons 'and (cddr expr))
                               #f)
                         env))))
          ((and (pair? expr)
                (eq? (car expr) 'or))
           (cond ((null? (cdr expr))
                  (parse use
                         #f
                         env))
                 ((null? (cddr expr))
                  (parse use
                         (cadr expr)
                         env))
                 ((eq? use 'test)
                  (parse use
                         (list 'if
                               (cadr expr)
                               #t
                               (cons 'or (cddr expr)))
                         env))
                 (else
                  (parse use
                         (let ((v (gensym)))
                           (list 'let
                                 (list (list v (cadr expr)))
                                 (list 'if
                                       v
                                       v
                                       (cons 'or (cddr expr)))))
                         env))))
	  ;; primitive substitution here
	  ;; TODO do this optimization in the following pass instead of at parse time ?
	  ((and (pair? expr)
		(assoc (car expr) substitute-primitives))
	   =>
	   (lambda (prim)
	     (parse use
		    (cons (cdr prim) (cdr expr))
		    env)))
	  ;; binary arthimetic operations can use primitives directly
	  ((and (pair? expr)
		(= (length (cdr expr)) 2)
		(assoc (car expr) '((+ . #%+) (- . #%-) (* . #%*))))
	   =>
	   (lambda (prim)
	     (parse use
		    (cons (cdr prim) (cdr expr))
		    env)))
          ((and (pair? expr)
                (memq (car expr)
                      '(quote quasiquote unquote unquote-splicing lambda if
                        set! cond and or case let let* letrec begin do define
                        delay)))
           (compiler-error "the compiler does not implement the special form" (car expr)))
          ((pair? expr)
           (let* ((exprs (map (lambda (x) (parse 'value x env)) expr))
                  (r (make-call #f exprs)))
             (for-each (lambda (x) (node-parent-set! x r)) exprs)
             r))
          (else
           (compiler-error "unknown expression" expr)))))

(define parse-body
  (lambda (exprs env)
    (parse 'value (cons 'begin exprs) env)))

(define self-eval?
  (lambda (expr)
    (or (number? expr)
        (char? expr)
        (boolean? expr)
        (string? expr))))

(define extract-ids
  (lambda (pattern)
    (if (pair? pattern)
        (cons (car pattern) (extract-ids (cdr pattern)))
        (if (symbol? pattern)
            (cons pattern '())
            '()))))

(define has-rest-param?
  (lambda (pattern)
    (if (pair? pattern)
        (has-rest-param? (cdr pattern))
        (symbol? pattern))))

(define (adjust-unmutable-references! node)
  '(pretty-print (list unmut: (node->expr node)))
  (if (and (call? node)
           '(display "call ")
           (ref? (car (node-children node)))
           '(display "ref ")
           (eq? '#%unbox (var-id (ref-var (car (node-children node)))))
           '(display "unbox")
           (ref? (cadr (node-children node)))
           '(display "ref ")
           (not (mutable-var? (ref-var (cadr (node-children node)))))
           '(display "unmut! ")) 
      (let* ((parent (node-parent node)) (child (cadr (node-children node))))
        (node-parent-set! child parent)
        (if parent
            (node-children-set! parent
				(map (lambda (c) (if (eq? c node) child c))
				     (node-children parent))))
        child)
      (begin (for-each (lambda (n) (adjust-unmutable-references! n))
		       (node-children node))
             node)))

;-----------------------------------------------------------------------------

;; Compilation context representation.

(define-type context
  code
  env
  env2
)

(define context-change-code
  (lambda (ctx code)
    (make-context code
                  (context-env ctx)
                  (context-env2 ctx))))

(define context-change-env
  (lambda (ctx env)
    (make-context (context-code ctx)
                  env
                  (context-env2 ctx))))

(define context-change-env2
  (lambda (ctx env2)
    (make-context (context-code ctx)
                  (context-env ctx)
                  env2)))

(define make-init-context
  (lambda ()
    (make-context (make-init-code)
                  (make-init-env)
                  #f)))

(define context-make-label
  (lambda (ctx)
    (context-change-code ctx (code-make-label (context-code ctx)))))

(define context-last-label
  (lambda (ctx)
    (code-last-label (context-code ctx))))

(define context-add-bb
  (lambda (ctx label)
    (context-change-code ctx (code-add-bb (context-code ctx) label))))

(define context-add-instr
  (lambda (ctx instr)
    (context-change-code ctx (code-add-instr (context-code ctx) instr))))

;; Representation of code.

(define-type code
  last-label
  rev-bbs
)

(define-type bb
  label
  rev-instrs
)

(define make-init-code
  (lambda ()
    (make-code 0
               (list (make-bb 0 (list))))))

(define code-make-label
  (lambda (code)
    (let ((label (+ (code-last-label code) 1)))
      (make-code label
                 (code-rev-bbs code)))))

(define code-add-bb
  (lambda (code label)
    (make-code
     (code-last-label code)
     (cons (make-bb label '())
           (code-rev-bbs code)))))

(define code-add-instr
  (lambda (code instr)
    (let* ((rev-bbs (code-rev-bbs code))
           (bb (car rev-bbs))
           (rev-instrs (bb-rev-instrs bb)))
      (make-code
       (code-last-label code)
       (cons (make-bb (bb-label bb)
                      (cons instr rev-instrs))
             (cdr rev-bbs))))))

;; Representation of compile-time stack.

(define-type stack
  size  ; number of slots
  slots ; for each slot, the variable (or #f) contained in the slot
)

(define make-init-stack
  (lambda ()
    (make-stack 0 '())))

(define stack-extend
  (lambda (x nb-slots stk)
    (let ((size (stack-size stk)))
      (make-stack
       (+ size nb-slots)
       (append (repeat nb-slots x) (stack-slots stk))))))

(define stack-discard
  (lambda (nb-slots stk)
    (let ((size (stack-size stk)))
      (make-stack
       (- size nb-slots)
       (list-tail (stack-slots stk) nb-slots)))))

;; Representation of compile-time environment.

(define-type env
  local
  closed
)

(define make-init-env
  (lambda ()
    (make-env (make-init-stack)
              '())))

(define env-change-local
  (lambda (env local)
    (make-env local
              (env-closed env))))

(define env-change-closed
  (lambda (env closed)
    (make-env (env-local env)
              closed)))

(define find-local-var
  (lambda (var env)
    (let ((i (pos-in-list var (stack-slots (env-local env)))))
      (or i
          (- (+ (pos-in-list var (env-closed env)) 1))))))

(define prc->env
  (lambda (prc)
    (make-env
     (let ((params (prc-params prc)))
       (make-stack (length params)
                   (append (map var-id params) '())))
     (let ((vars (varset->list (non-global-fv prc))))
;       (pp (map var-id vars))
       (map var-id vars)))))

;-----------------------------------------------------------------------------

(define gen-instruction
  (lambda (instr nb-pop nb-push ctx)
    (let* ((env
            (context-env ctx))
           (stk
            (stack-extend #f
                          nb-push
                          (stack-discard nb-pop
                                         (env-local env)))))
      (context-add-instr (context-change-env ctx (env-change-local env stk))
                         instr))))

(define gen-entry
  (lambda (nparams rest? ctx)
    (gen-instruction (list 'entry nparams rest?) 0 0 ctx)))

(define gen-push-constant
  (lambda (val ctx)
    (gen-instruction (list 'push-constant val) 0 1 ctx)))

(define gen-push-unspecified
  (lambda (ctx)
    (gen-push-constant #f ctx)))

(define gen-push-local-var
  (lambda (var ctx)
;    (pp (list var: var local: (stack-slots (env-local (context-env ctx))) (env-closed (context-env ctx))))
    (let ((i (find-local-var var (context-env ctx))))
      (if (>= i 0)
          (gen-push-stack i ctx)
          (gen-push-stack
	   ;; this +1 is needed because closures are in the environment, but
	   ;; don't contain a value, and must therefore be skipped
	   (+ 1
	      (- -1 i)
	      (length (stack-slots (env-local (context-env ctx))))) ctx)))))

(define gen-push-stack
  (lambda (pos ctx)
    (gen-instruction (list 'push-stack pos) 0 1 ctx)))

(define gen-push-global
  (lambda (var ctx)
    (gen-instruction (list 'push-global var) 0 1 ctx)))

(define gen-set-global
  (lambda (var ctx)
    (gen-instruction (list 'set-global var) 1 0 ctx)))

(define gen-call
  (lambda (nargs ctx)
    (gen-instruction (list 'call nargs) (+ nargs 1) 1 ctx)))

(define gen-jump
  (lambda (nargs ctx)
    (gen-instruction (list 'jump nargs) (+ nargs 1) 1 ctx)))

(define gen-call-toplevel
  (lambda (nargs id ctx)
    (gen-instruction (list 'call-toplevel id) nargs 1 ctx)))

(define gen-jump-toplevel
  (lambda (nargs id ctx)
    (gen-instruction (list 'jump-toplevel id) nargs 1 ctx)))

(define gen-goto
  (lambda (label ctx)
    (gen-instruction (list 'goto label) 0 0 ctx)))

(define gen-goto-if-false
  (lambda (label-false label-true ctx)
    (gen-instruction (list 'goto-if-false label-false label-true) 1 0 ctx)))

(define gen-closure
  (lambda (label-entry ctx)
    (gen-instruction (list 'closure label-entry) 1 1 ctx)))

(define gen-prim
  (lambda (id nargs unspec-result? ctx)
    (gen-instruction
     (list 'prim id)
     nargs
     (if unspec-result? 0 1)
     ctx)))

(define gen-shift
  (lambda (n ctx)
    (if (> n 0)
        (gen-instruction (list 'shift) 1 0 (gen-shift (- n 1) ctx))
        ctx)))

(define gen-pop
  (lambda (ctx)
    (gen-instruction (list 'pop) 1 0 ctx)))

(define gen-return
  (lambda (ctx)
    (let ((ss (stack-size (env-local (context-env ctx)))))
      (gen-instruction (list 'return) ss 0 ctx))))

;-----------------------------------------------------------------------------

(define child1
  (lambda (node)
    (car (node-children node))))

(define child2
  (lambda (node)
    (cadr (node-children node))))

(define child3
  (lambda (node)
    (caddr (node-children node))))

(define comp-none
  (lambda (node ctx)

    (cond ((or (cst? node)
               (ref? node)
               (prc? node))
           ctx)

          ((def? node)
           (let ((var (def-var node)))
             (if (toplevel-prc-with-non-rest-correct-calls? var)
                 (comp-prc (child1 node) #f ctx)
                 (if (var-needed? var)
                     (let ((ctx2 (comp-push (child1 node) ctx)))
                       (gen-set-global (var-id var) ctx2))
                     (comp-none (child1 node) ctx)))))

          ((set? node)
           (let ((var (set-var node)))
             (if (var-needed? var)
                 (let ((ctx2 (comp-push (child1 node) ctx)))
                   (gen-set-global (var-id var) ctx2))
                 (comp-none (child1 node) ctx))))

          ((if? node)
           (let* ((ctx2
                   (context-make-label ctx))
                  (label-then
                   (context-last-label ctx2))
                  (ctx3
                   (context-make-label ctx2))
                  (label-else
                   (context-last-label ctx3))
                  (ctx4
                   (context-make-label ctx3))
                  (label-then-join
                   (context-last-label ctx4))
                  (ctx5
                   (context-make-label ctx4))
                  (label-else-join
                   (context-last-label ctx5))
                  (ctx6
                   (context-make-label ctx5))
                  (label-join
                   (context-last-label ctx6))
                  (ctx7
                   (comp-test (child1 node) label-then label-else ctx6))
                  (ctx8
                   (gen-goto
                    label-else-join
                    (comp-none (child3 node)
                               (context-change-env2
                                (context-add-bb ctx7 label-else)
                                #f))))
                  (ctx9
                   (gen-goto
                    label-then-join
                    (comp-none (child2 node)
                               (context-change-env
                                (context-add-bb ctx8 label-then)
                                (context-env2 ctx7)))))
                  (ctx10
                   (gen-goto
                    label-join
                    (context-add-bb ctx9 label-else-join)))
                  (ctx11
                   (gen-goto
                    label-join
                    (context-add-bb ctx10 label-then-join)))
                  (ctx12
                   (context-add-bb ctx11 label-join)))
             ctx12))

          ((call? node)
           (comp-call node 'none ctx))

          ((seq? node)
           (let ((children (node-children node)))
             (if (null? children)
                 ctx
                 (let loop ((lst children)
                            (ctx ctx))
                   (if (null? (cdr lst))
                       (comp-none (car lst) ctx)
                       (loop (cdr lst)
                             (comp-none (car lst) ctx)))))))

          (else
           (compiler-error "unknown expression type" node)))))

(define comp-tail
  (lambda (node ctx)

    (cond ((or (cst? node)
               (ref? node)
               (def? node)
               (set? node)
               (prc? node)
;               (call? node)
               )
           (gen-return (comp-push node ctx)))

          ((if? node)
           (let* ((ctx2
                   (context-make-label ctx))
                  (label-then
                   (context-last-label ctx2))
                  (ctx3
                   (context-make-label ctx2))
                  (label-else
                   (context-last-label ctx3))
                  (ctx4
                   (comp-test (child1 node) label-then label-else ctx3))
                  (ctx5
                   (comp-tail (child3 node)
                              (context-change-env2
                               (context-add-bb ctx4 label-else)
                               #f)))
                  (ctx6
                   (comp-tail (child2 node)
                              (context-change-env
                               (context-add-bb ctx5 label-then)
                               (context-env2 ctx4)))))
             ctx6))

          ((call? node)
           (comp-call node 'tail ctx))

          ((seq? node)
           (let ((children (node-children node)))
             (if (null? children)
                 (gen-return (gen-push-unspecified ctx))
                 (let loop ((lst children)
                            (ctx ctx))
                   (if (null? (cdr lst))
                       (comp-tail (car lst) ctx)
                       (loop (cdr lst)
                             (comp-none (car lst) ctx)))))))

          (else
           (compiler-error "unknown expression type" node)))))

(define comp-push
  (lambda (node ctx)

    '(
    (display "--------------\n")
    (pp (node->expr node))
    (pp env)
    (pp stk)
     )

    (cond ((cst? node)
           (let ((val (cst-val node)))
             (gen-push-constant val ctx)))

          ((ref? node)
           (let ((var (ref-var node)))
             (if (var-global? var)
                 (if (null? (var-defs var))
                     (compiler-error "undefined variable:" (var-id var))
		     (let ((val (child1 (car (var-defs var)))))
		       (if (and (not (mutable-var? var))
				(cst? val)) ;; immutable global, counted as cst
			   (gen-push-constant (cst-val val) ctx)
			   (gen-push-global (var-id var) ctx))))
                 (gen-push-local-var (var-id var) ctx))))

          ((or (def? node)
               (set? node))
           (gen-push-unspecified (comp-none node ctx)))

          ((if? node)
           (let* ((ctx2
                   (context-make-label ctx))
                  (label-then
                   (context-last-label ctx2))
                  (ctx3
                   (context-make-label ctx2))
                  (label-else
                   (context-last-label ctx3))
                  (ctx4
                   (context-make-label ctx3))
                  (label-then-join
                   (context-last-label ctx4))
                  (ctx5
                   (context-make-label ctx4))
                  (label-else-join
                   (context-last-label ctx5))
                  (ctx6
                   (context-make-label ctx5))
                  (label-join
                   (context-last-label ctx6))
                  (ctx7
                   (comp-test (child1 node) label-then label-else ctx6))
                  (ctx8
                   (gen-goto
                    label-else-join
                    (comp-push (child3 node)
                               (context-change-env2
                                (context-add-bb ctx7 label-else)
                                #f))))
                  (ctx9
                   (gen-goto
                    label-then-join
                    (comp-push (child2 node)
                               (context-change-env
                                (context-add-bb ctx8 label-then)
                                (context-env2 ctx7)))))
                  (ctx10
                   (gen-goto
                    label-join
                    (context-add-bb ctx9 label-else-join)))
                  (ctx11
                   (gen-goto
                    label-join
                    (context-add-bb ctx10 label-then-join)))
                  (ctx12
                   (context-add-bb ctx11 label-join)))
             ctx12))

          ((prc? node)
           (comp-prc node #t ctx))

          ((call? node)
           (comp-call node 'push ctx))

          ((seq? node)
           (let ((children (node-children node)))
             (if (null? children)
                 (gen-push-unspecified ctx)
                 (let loop ((lst children)
                            (ctx ctx))
                   (if (null? (cdr lst))
                       (comp-push (car lst) ctx)
                       (loop (cdr lst)
                             (comp-none (car lst) ctx)))))))

          (else
           (compiler-error "unknown expression type" node)))))

(define (build-closure label-entry vars ctx)

  (define (build vars ctx)
    (if (null? vars)
        (gen-push-constant '() ctx)
        (gen-prim '#%cons
                  2
                  #f
                  (build (cdr vars)
                         (gen-push-local-var (car vars) ctx)))))

  (if (null? vars)
      (gen-closure label-entry
                   (gen-push-constant '() ctx))
      (gen-closure label-entry
                   (build vars ctx))))

(define comp-prc
  (lambda (node closure? ctx)
    (let* ((ctx2
            (context-make-label ctx))
           (label-entry
            (context-last-label ctx2))
           (ctx3
            (context-make-label ctx2))
           (label-continue
            (context-last-label ctx3))
           (body-env
            (prc->env node))
           (ctx4
            (if closure?
                (build-closure label-entry (env-closed body-env) ctx3)
                ctx3))
           (ctx5
            (gen-goto label-continue ctx4))
           (ctx6
            (gen-entry (length (prc-params node))
                       (prc-rest? node)
                       (context-add-bb (context-change-env ctx5
                                                           body-env)
                                       label-entry)))
           (ctx7
            (comp-tail (child1 node) ctx6)))
      (prc-entry-label-set! node label-entry)
      (context-add-bb (context-change-env ctx7 (context-env ctx5))
                      label-continue))))

(define comp-call
  (lambda (node reason ctx)
    (let* ((op (child1 node))
           (args (cdr (node-children node)))
           (nargs (length args)))
      (let loop ((lst args)
                 (ctx ctx))
        (if (pair? lst)

            (let ((arg (car lst)))
              (loop (cdr lst)
                    (comp-push arg ctx)))

            (cond ((and (ref? op)
                        (var-primitive (ref-var op)))
                   (let* ((var (ref-var op))
                          (id (var-id var))
                          (primitive (var-primitive var))
                          (prim-nargs (primitive-nargs primitive)))

                     (define use-result
                       (lambda (ctx2)
                         (cond ((eq? reason 'tail)
                                (gen-return
                                 (if (primitive-unspecified-result? primitive)
                                     (gen-push-unspecified ctx2)
                                     ctx2)))
                               ((eq? reason 'push)
                                (if (primitive-unspecified-result? primitive)
                                    (gen-push-unspecified ctx2)
                                    ctx2))
                               (else
                                (if (primitive-unspecified-result? primitive)
                                    ctx2
                                    (gen-pop ctx2))))))

                     (use-result
                      (if (primitive-inliner primitive)
                          ((primitive-inliner primitive) ctx)
                          (if
			   (not (= nargs prim-nargs))
			   (compiler-error
			    "primitive called with wrong number of arguments"
			    id)
			   (gen-prim
			    id
			    prim-nargs
			    (primitive-unspecified-result? primitive)
			    ctx))))))
		  
		  
                  ((and (ref? op)
                        (toplevel-prc-with-non-rest-correct-calls?
			 (ref-var op)))
                   =>
                   (lambda (prc)
                     (cond ((eq? reason 'tail)
                            (gen-jump-toplevel nargs prc ctx))
                           ((eq? reason 'push)
                            (gen-call-toplevel nargs prc ctx))
                           (else
                            (gen-pop (gen-call-toplevel nargs prc ctx))))))

                  (else
                   (let ((ctx2 (comp-push op ctx)))
                     (cond ((eq? reason 'tail)
                            (gen-jump nargs ctx2))
                           ((eq? reason 'push)
                            (gen-call nargs ctx2))
                           (else
                            (gen-pop (gen-call nargs ctx2))))))))))))

(define comp-test
  (lambda (node label-true label-false ctx)
    (cond ((cst? node)
           (let ((ctx2
                  (gen-goto
                   (let ((val (cst-val node)))
                     (if val
                         label-true
                         label-false))
                   ctx)))
             (context-change-env2 ctx2 (context-env ctx2))))

          ((or (ref? node)
               (def? node)
               (set? node)
               (if? node)
               (call? node)
               (seq? node))
           (let* ((ctx2
                   (comp-push node ctx))
                  (ctx3
                   (gen-goto-if-false label-false label-true ctx2)))
             (context-change-env2 ctx3 (context-env ctx3))))

          ((prc? node)
           (let ((ctx2
                  (gen-goto label-true ctx)))
             (context-change-env2 ctx2 (context-env ctx2))))

          (else
           (compiler-error "unknown expression type" node)))))

;-----------------------------------------------------------------------------

(define toplevel-prc?
  (lambda (var)
    (and (not (mutable-var? var))
         (let ((d (var-defs var)))
           (and (pair? d)
                (null? (cdr d))
                (let ((val (child1 (car d))))
                  (and (prc? val)
                       val)))))))

(define toplevel-prc-with-non-rest-correct-calls?
  (lambda (var)
    (let ((prc (toplevel-prc? var)))
      (and prc
           (not (prc-rest? prc))
           (every (lambda (r)
                    (let ((parent (node-parent r)))
                      (and (call? parent)
                           (eq? (child1 parent) r)
                           (= (length (prc-params prc))
                              (- (length (node-children parent)) 1)))))
                  (var-refs var))
           prc))))

(define mutable-var?
  (lambda (var)
    (not (null? (var-sets var)))))

(define global-fv
  (lambda (node)
    (list->varset
     (keep var-global?
           (varset->list (fv node))))))

(define non-global-fv
  (lambda (node)
    (list->varset
     (keep (lambda (x) (not (var-global? x)))
           (varset->list (fv node))))))

(define fv
  (lambda (node)
    (cond ((cst? node)
           (varset-empty))
          ((ref? node)
           (let ((var (ref-var node)))
             (varset-singleton var)))
          ((def? node)
           (let ((var (def-var node))
                 (val (child1 node)))
             (varset-union
              (varset-singleton var)
              (fv val))))
          ((set? node)
           (let ((var (set-var node))
                 (val (child1 node)))
             (varset-union
              (varset-singleton var)
              (fv val))))
          ((if? node)
           (let ((a (list-ref (node-children node) 0))
                 (b (list-ref (node-children node) 1))
                 (c (list-ref (node-children node) 2)))
             (varset-union-multi (list (fv a) (fv b) (fv c)))))
          ((prc? node)
           (let ((body (list-ref (node-children node) 0)))
             (varset-difference
              (fv body)
              (build-params-varset (prc-params node)))))
          ((call? node)
           (varset-union-multi (map fv (node-children node))))
          ((seq? node)
           (varset-union-multi (map fv (node-children node))))
          (else
           (compiler-error "unknown expression type" node)))))

(define build-params-varset
  (lambda (params)
    (list->varset params)))

(define mark-needed-global-vars!
  (lambda (global-env node)

    (define readyq
      (env-lookup global-env '#%readyq))

    (define mark-var!
      (lambda (var)
        (if (and (var-global? var)
                 (not (var-needed? var))
		 ;; globals that obey the following conditions are considered
		 ;; to be constants
		 (not (and (not (mutable-var? var))
			   ;; to weed out primitives, which have no definitions
			   (> (length (var-defs var)) 0)
			   (cst? (child1 (car (var-defs var)))))))
            (begin
              (var-needed?-set! var #t)
              (for-each
               (lambda (def)
                 (let ((val (child1 def)))
                   (if (side-effect-less? val)
                       (mark! val))))
               (var-defs var))
              (if (eq? var readyq)
                  (begin
                    (mark-var!
                     (env-lookup global-env '#%start-first-process))
                    (mark-var!
                     (env-lookup global-env '#%exit))))))))

    (define side-effect-less?
      (lambda (node)
        (or (cst? node)
            (ref? node)
            (prc? node))))

    (define mark!
      (lambda (node)
        (cond ((cst? node))
              ((ref? node)
               (let ((var (ref-var node)))
                 (mark-var! var)))
              ((def? node)
               (let ((var (def-var node))
                     (val (child1 node)))
                 (if (not (side-effect-less? val))
                     (mark! val))))
              ((set? node)
               (let ((var (set-var node))
                     (val (child1 node)))
                 (mark! val)))
              ((if? node)
               (let ((a (list-ref (node-children node) 0))
                     (b (list-ref (node-children node) 1))
                     (c (list-ref (node-children node) 2)))
                 (mark! a)
                 (mark! b)
                 (mark! c)))
              ((prc? node)
               (let ((body (list-ref (node-children node) 0)))
                 (mark! body)))
              ((call? node)
               (for-each mark! (node-children node)))
              ((seq? node)
               (for-each mark! (node-children node)))
              (else
               (compiler-error "unknown expression type" node)))))

    (mark! node)
))

;-----------------------------------------------------------------------------

;; Variable sets

(define (varset-empty)              ; return the empty set
  '())

(define (varset-singleton x)        ; create a set containing only 'x'
  (list x))

(define (list->varset lst)          ; convert list to set
  lst)

(define (varset->list set)          ; convert set to list
  set)

(define (varset-size set)           ; return cardinality of set
  (list-length set))

(define (varset-empty? set)         ; is 'x' the empty set?
  (null? set))

(define (varset-member? x set)      ; is 'x' a member of the 'set'?
  (and (not (null? set))
       (or (eq? x (car set))
           (varset-member? x (cdr set)))))

(define (varset-adjoin set x)       ; add the element 'x' to the 'set'
  (if (varset-member? x set) set (cons x set)))

(define (varset-remove set x)       ; remove the element 'x' from 'set'
  (cond ((null? set)
         '())
        ((eq? (car set) x)
         (cdr set))
        (else
         (cons (car set) (varset-remove (cdr set) x)))))

(define (varset-equal? s1 s2)       ; are 's1' and 's2' equal sets?
  (and (varset-subset? s1 s2)
       (varset-subset? s2 s1)))

(define (varset-subset? s1 s2)      ; is 's1' a subset of 's2'?
  (cond ((null? s1)
         #t)
        ((varset-member? (car s1) s2)
         (varset-subset? (cdr s1) s2))
        (else
         #f)))

(define (varset-difference set1 set2) ; return difference of sets
  (cond ((null? set1)
         '())
        ((varset-member? (car set1) set2)
         (varset-difference (cdr set1) set2))
        (else
         (cons (car set1) (varset-difference (cdr set1) set2)))))

(define (varset-union set1 set2)    ; return union of sets
  (define (union s1 s2)
    (cond ((null? s1)
           s2)
          ((varset-member? (car s1) s2)
           (union (cdr s1) s2))
          (else
           (cons (car s1) (union (cdr s1) s2)))))
  (if (varset-smaller? set1 set2)
    (union set1 set2)
    (union set2 set1)))

(define (varset-intersection set1 set2) ; return intersection of sets
  (define (intersection s1 s2)
    (cond ((null? s1)
           '())
          ((varset-member? (car s1) s2)
           (cons (car s1) (intersection (cdr s1) s2)))
          (else
           (intersection (cdr s1) s2))))
  (if (varset-smaller? set1 set2)
    (intersection set1 set2)
    (intersection set2 set1)))

(define (varset-intersects? set1 set2) ; do sets 'set1' and 'set2' intersect?
  (not (varset-empty? (varset-intersection set1 set2))))

(define (varset-smaller? set1 set2)
  (if (null? set1)
    (not (null? set2))
    (if (null? set2)
      #f
      (varset-smaller? (cdr set1) (cdr set2)))))

(define (varset-union-multi sets)
  (if (null? sets)
    (varset-empty)
    (n-ary varset-union (car sets) (cdr sets))))

(define (n-ary function first rest)
  (if (null? rest)
    first
    (n-ary function (function first (car rest)) (cdr rest))))

;------------------------------------------------------------------------------

(define code->vector
  (lambda (code)
    (let ((v (make-vector (+ (code-last-label code) 1))))
      (for-each
       (lambda (bb)
         (vector-set! v (bb-label bb) bb))
       (code-rev-bbs code))
      v)))

(define bbs->ref-counts
  (lambda (bbs)
    (let ((ref-counts (make-vector (vector-length bbs) 0)))

      (define visit
        (lambda (label)
          (let ((ref-count (vector-ref ref-counts label)))
            (vector-set! ref-counts label (+ ref-count 1))
            (if (= ref-count 0)
                (let* ((bb (vector-ref bbs label))
                       (rev-instrs (bb-rev-instrs bb)))
                  (for-each
                   (lambda (instr)
                     (let ((opcode (car instr)))
                       (cond ((eq? opcode 'goto)
                              (visit (cadr instr)))
                             ((eq? opcode 'goto-if-false)
                              (visit (cadr instr))
                              (visit (caddr instr)))
                             ((or (eq? opcode 'closure)
                                  (eq? opcode 'call-toplevel)
                                  (eq? opcode 'jump-toplevel))
                              (visit (cadr instr))))))
                   rev-instrs))))))

      (visit 0)

      ref-counts)))

(define resolve-toplevel-labels!
  (lambda (bbs)
    (let loop ((i 0))
      (if (< i (vector-length bbs))
          (let* ((bb (vector-ref bbs i))
                 (rev-instrs (bb-rev-instrs bb)))
            (bb-rev-instrs-set!
             bb
             (map (lambda (instr)
                    (let ((opcode (car instr)))
                      (cond ((eq? opcode 'call-toplevel)
                             (list opcode
                                   (prc-entry-label (cadr instr))))
                            ((eq? opcode 'jump-toplevel)
                             (list opcode
                                   (prc-entry-label (cadr instr))))
                            (else
                             instr))))
                  rev-instrs))
            (loop (+ i 1)))))))

(define tighten-jump-cascades!
  (lambda (bbs)
    (let ((ref-counts (bbs->ref-counts bbs)))

      (define resolve
        (lambda (label)
          (let* ((bb (vector-ref bbs label))
                 (rev-instrs (bb-rev-instrs bb)))
            (and (or (null? (cdr rev-instrs))
                     (= (vector-ref ref-counts label) 1))
                 rev-instrs))))

      (let loop1 ()
        (let loop2 ((i 0)
                    (changed? #f))
          (if (< i (vector-length bbs))
              (if (> (vector-ref ref-counts i) 0)
                  (let* ((bb (vector-ref bbs i))
                         (rev-instrs (bb-rev-instrs bb))
                         (jump (car rev-instrs))
                         (opcode (car jump)))
                    (cond ((eq? opcode 'goto)
                           (let* ((label (cadr jump))
                                  (jump-replacement (resolve label)))
                             (if jump-replacement
                                 (begin
                                   (vector-set!
                                    bbs
                                    i
                                    (make-bb (bb-label bb)
                                             (append jump-replacement
                                                     (cdr rev-instrs))))
                                   (loop2 (+ i 1)
                                          #t))
                                 (loop2 (+ i 1)
                                        changed?))))
                          ((eq? opcode 'goto-if-false)
                           (let* ((label-then (cadr jump))
                                  (label-else (caddr jump))
                                  (jump-then-replacement (resolve label-then))
                                  (jump-else-replacement (resolve label-else)))
                             (if (and jump-then-replacement
                                      (null? (cdr jump-then-replacement))
                                      jump-else-replacement
                                      (null? (cdr jump-else-replacement))
                                      (or (eq? (caar jump-then-replacement)
					       'goto)
                                          (eq? (caar jump-else-replacement)
					       'goto)))
                                 (begin
                                   (vector-set!
                                    bbs
                                    i
                                    (make-bb
				     (bb-label bb)
				     (cons
				      (list
				       'goto-if-false
				       (if (eq? (caar jump-then-replacement)
						'goto)
					   (cadar jump-then-replacement)
					   label-then)
				       (if (eq? (caar jump-else-replacement)
						'goto)
					   (cadar jump-else-replacement)
					   label-else))
				      (cdr rev-instrs))))
                                   (loop2 (+ i 1)
                                          #t))
                                 (loop2 (+ i 1)
                                        changed?))))
                          (else
                           (loop2 (+ i 1)
                                  changed?))))
                  (loop2 (+ i 1)
                         changed?))
              (if changed?
                  (loop1))))))))

(define remove-useless-bbs!
  (lambda (bbs)
    (let ((ref-counts (bbs->ref-counts bbs)))
      (let loop1 ((label 0) (new-label 0))
        (if (< label (vector-length bbs))
            (if (> (vector-ref ref-counts label) 0)
                (let ((bb (vector-ref bbs label)))
                  (vector-set!
                   bbs
                   label
                   (make-bb new-label (bb-rev-instrs bb)))
                  (loop1 (+ label 1) (+ new-label 1)))
                (loop1 (+ label 1) new-label))
            (renumber-labels bbs ref-counts new-label))))))

(define renumber-labels
  (lambda (bbs ref-counts n)
    (let ((new-bbs (make-vector n)))
      (let loop2 ((label 0))
        (if (< label (vector-length bbs))
            (if (> (vector-ref ref-counts label) 0)
                (let* ((bb (vector-ref bbs label))
                       (new-label (bb-label bb))
                       (rev-instrs (bb-rev-instrs bb)))

                  (define fix
                    (lambda (instr)

                      (define new-label
                        (lambda (label)
                          (bb-label (vector-ref bbs label))))

                      (let ((opcode (car instr)))
                        (cond ((eq? opcode 'closure)
                               (list 'closure
                                     (new-label (cadr instr))))
                              ((eq? opcode 'call-toplevel)
                               (list 'call-toplevel
                                     (new-label (cadr instr))))
                              ((eq? opcode 'jump-toplevel)
                               (list 'jump-toplevel
                                     (new-label (cadr instr))))
                              ((eq? opcode 'goto)
                               (list 'goto
                                     (new-label (cadr instr))))
                              ((eq? opcode 'goto-if-false)
                               (list 'goto-if-false
                                     (new-label (cadr instr))
                                     (new-label (caddr instr))))
                              (else
                               instr)))))

                  (vector-set!
                   new-bbs
                   new-label
                   (make-bb new-label (map fix rev-instrs)))
                  (loop2 (+ label 1)))
                (loop2 (+ label 1)))
            new-bbs)))))

(define reorder!
  (lambda (bbs)
    (let* ((done (make-vector (vector-length bbs) #f)))

      (define unscheduled?
        (lambda (label)
          (not (vector-ref done label))))

      (define label-refs
        (lambda (instrs todo)
          (if (pair? instrs)
              (let* ((instr (car instrs))
                     (opcode (car instr)))
                (cond ((or (eq? opcode 'closure)
                           (eq? opcode 'call-toplevel)
                           (eq? opcode 'jump-toplevel))
                       (label-refs (cdr instrs) (cons (cadr instr) todo)))
                      (else
                       (label-refs (cdr instrs) todo))))
              todo)))

      (define schedule-here
        (lambda (label new-label todo cont)
          (let* ((bb (vector-ref bbs label))
                 (rev-instrs (bb-rev-instrs bb))
                 (jump (car rev-instrs))
                 (opcode (car jump))
                 (new-todo (label-refs rev-instrs todo)))
            (vector-set! bbs label (make-bb new-label rev-instrs))
            (vector-set! done label #t)
            (cond ((eq? opcode 'goto)
                   (let ((label (cadr jump)))
                     (if (unscheduled? label)
                         (schedule-here label
                                        (+ new-label 1)
                                        new-todo
                                        cont)
                         (cont (+ new-label 1)
                               new-todo))))
                  ((eq? opcode 'goto-if-false)
                   (let ((label-then (cadr jump))
                         (label-else (caddr jump)))
                     (cond ((unscheduled? label-else)
                            (schedule-here label-else
                                           (+ new-label 1)
                                           (cons label-then new-todo)
                                           cont))
                           ((unscheduled? label-then)
                            (schedule-here label-then
                                           (+ new-label 1)
                                           new-todo
                                           cont))
                           (else
                            (cont (+ new-label 1)
                                  new-todo)))))
                  (else
                   (cont (+ new-label 1)
                         new-todo))))))

      (define schedule-somewhere
        (lambda (label new-label todo cont)
          (schedule-here label new-label todo cont)))

      (define schedule-todo
        (lambda (new-label todo)
          (if (pair? todo)
              (let ((label (car todo)))
                (if (unscheduled? label)
                    (schedule-somewhere label
                                        new-label
                                        (cdr todo)
                                        schedule-todo)
                    (schedule-todo new-label
                                   (cdr todo)))))))


      (schedule-here 0 0 '() schedule-todo)

      (renumber-labels bbs
                       (make-vector (vector-length bbs) 1)
                       (vector-length bbs)))))

(define linearize
  (lambda (bbs)
    (let loop ((label (- (vector-length bbs) 1))
               (lst '()))
      (if (>= label 0)
          (let* ((bb (vector-ref bbs label))
                 (rev-instrs (bb-rev-instrs bb))
                 (jump (car rev-instrs))
                 (opcode (car jump)))
            (loop (- label 1)
                  (append
                   (list label)
                   (reverse
                    (cond ((eq? opcode 'goto)
                           (if (= (cadr jump) (+ label 1))
                               (cdr rev-instrs)
                               rev-instrs))
                          ((eq? opcode 'goto-if-false)
                           (cond ((= (caddr jump) (+ label 1))
                                  (cons (list 'goto-if-false (cadr jump))
                                        (cdr rev-instrs)))
                                 ((= (cadr jump) (+ label 1))
                                  (cons (list 'goto-if-not-false (caddr jump))
                                        (cdr rev-instrs)))
                                 (else
                                  (cons (list 'goto (caddr jump))
                                        (cons (list 'goto-if-false (cadr jump))
                                              (cdr rev-instrs))))))
                          (else
                           rev-instrs)))
                   lst)))
          lst))))

(define optimize-code
  (lambda (code)
    (let ((bbs (code->vector code)))
      (resolve-toplevel-labels! bbs)
      (tighten-jump-cascades! bbs)
      (let ((bbs (remove-useless-bbs! bbs)))
        (reorder! bbs)))))


(define expand-includes
  (lambda (exprs)
    (map (lambda (e)
	   (if (eq? (car e) 'include)
	       (cons 'begin
		     (expand-includes
		      (with-input-from-file (cadr e) read-all)))
	       e))
	 exprs)))

(define parse-file
  (lambda (filename)
    (let* ((library ;; TODO do not hard-code path
            (with-input-from-file "/home/vincent/src/picobit/dev/library.scm" read-all))
           (toplevel-exprs
            (expand-includes
	     (append library
		     (with-input-from-file filename read-all))))
           (global-env
            (make-global-env))
           (parsed-prog
            (parse-top (cons 'begin toplevel-exprs) global-env)))

      (for-each
       (lambda (node)
         (mark-needed-global-vars! global-env node))
       parsed-prog)

      (extract-parts
       parsed-prog
       (lambda (defs after-defs)

         (define make-seq-preparsed
           (lambda (exprs)
             (let ((r (make-seq #f exprs)))
               (for-each (lambda (x) (node-parent-set! x r)) exprs)
               r)))

         (define make-call-preparsed
           (lambda (exprs)
             (let ((r (make-call #f exprs)))
               (for-each (lambda (x) (node-parent-set! x r)) exprs)
               r)))

         (if (var-needed?
              (env-lookup global-env '#%readyq))
             (make-seq-preparsed
              (list (make-seq-preparsed defs)
                    (make-call-preparsed
                     (list (parse 'value '#%start-first-process global-env)
                           (let* ((pattern
                                   '())
                                  (ids
                                   (extract-ids pattern))
                                  (r
                                   (make-prc #f
					     '()
					     #f
					     (has-rest-param? pattern)
					     #f))
                                  (new-env
                                   (env-extend global-env ids r))
                                  (body
                                   (make-seq-preparsed after-defs)))
                             (prc-params-set!
                              r
                              (map (lambda (id) (env-lookup new-env id))
                                   ids))
                             (node-children-set! r (list body))
                             (node-parent-set! body r)
                             r)))
                    (parse 'value
                           '(#%exit)
                           global-env)))
             (make-seq-preparsed
              (append defs
                      after-defs
                      (list (parse 'value
                                   '(#%halt)
                                   global-env))))))))))

(define extract-parts
  (lambda (lst cont)
    (if (or (null? lst)
            (not (def? (car lst))))
        (cont '() lst)
        (extract-parts
         (cdr lst)
         (lambda (d ad)
           (cont (cons (car lst) d) ad))))))

;------------------------------------------------------------------------------

;;(include "asm.scm")

;;; File: "asm.scm"
;;;
;;; This module implements the generic assembler.

;;(##declare (standard-bindings) (fixnum) (block))

(define compiler-internal-error error)

;; (asm-begin! start-pos big-endian?) initializes the assembler and
;; starts a new empty code stream at address "start-pos".  It must be
;; called every time a new code stream is to be built.  The argument
;; "big-endian?" indicates the byte ordering to use for 16, 32 and 64
;; bit values.  After a call to "asm-begin!" the code stream is built
;; by calling the following procedures:
;;
;;  asm-8            to add an 8 bit integer to the code stream
;;  asm-16           to add a 16 bit integer to the code stream
;;  asm-32           to add a 32 bit integer to the code stream
;;  asm-64           to add a 64 bit integer to the code stream
;;  asm-float64      to add a 64 bit IEEE float to the code stream
;;  asm-string       to add a null terminated string to the code stream
;;  asm-label        to set a label to the current position in the code stream
;;  asm-align        to add enough zero bytes to force alignment
;;  asm-origin       to add enough zero bytes to move to a particular address
;;  asm-at-assembly  to defer code production to assembly time
;;  asm-listing      to add textual information to the listing

(define (asm-begin! start-pos big-endian?)
  (set! asm-start-pos start-pos)
  (set! asm-big-endian? big-endian?)
  (set! asm-code-stream (asm-make-stream))
  #f)

;; (asm-end!) must be called to finalize the assembler.

(define (asm-end!)
  (set! asm-code-stream #f)
  #f)

;; (asm-8 n) adds an 8 bit signed or unsigned integer to the code stream.

(define (asm-8 n)
  (asm-code-extend (asm-bits-0-to-7 n)))

;; (asm-16 n) adds a 16 bit signed or unsigned integer to the code stream.

(define (asm-16 n)
  (if asm-big-endian?
    (begin (asm-8 (asm-bits-8-and-up n)) (asm-8 n))
    (begin (asm-8 n) (asm-8 (asm-bits-8-and-up n)))))

;; (asm-32 n) adds a 32 bit signed or unsigned integer to the code stream.

(define (asm-32 n)
  (if asm-big-endian?
    (begin (asm-16 (asm-bits-16-and-up n)) (asm-16 n))
    (begin (asm-16 n) (asm-16 (asm-bits-16-and-up n)))))

;; (asm-64 n) adds a 64 bit signed or unsigned integer to the code stream.

(define (asm-64 n)
  (if asm-big-endian?
    (begin (asm-32 (asm-bits-32-and-up n)) (asm-32 n))
    (begin (asm-32 n) (asm-32 (asm-bits-32-and-up n)))))

;; (asm-float64 n) adds a 64 bit IEEE floating point number to the code stream.

(define (asm-float64 n)
  (asm-64 (asm-float->bits n)))

;; (asm-string str) adds a null terminated string to the code stream.

(define (asm-string str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
        (begin
          (asm-8 (char->integer (string-ref str i)))
          (loop (+ i 1)))
        (asm-8 0)))))

;; (asm-make-label id) creates a new label object.  A label can
;; be queried with "asm-label-pos" to obtain the label's position
;; relative to the start of the code stream (i.e. "start-pos").
;; The argument "id" gives a name to the label (not necessarily
;; unique) and is only needed for debugging purposes.

(define (asm-make-label id)
  (vector 'LABEL #f id))

;; (asm-label label-obj) sets the label to the current position in the
;; code stream.

(define (asm-label label-obj)
  (if (vector-ref label-obj 1)
    (compiler-internal-error
      "asm-label, label multiply defined" (asm-label-id label-obj))
    (begin
      (vector-set! label-obj 1 0)
      (asm-code-extend label-obj))))

;; (asm-label-id label-obj) returns the identifier of the label object.

(define (asm-label-id label-obj)
  (vector-ref label-obj 2))

;; (asm-label-pos label-obj) returns the position of the label
;; relative to the start of the code stream (i.e. "start-pos").
;; This procedure can only be called at assembly time (i.e.
;; within the call to "asm-assemble") or after assembly time
;; for labels declared prior to assembly time with "asm-label".
;; A label declared at assembly time can only be queried after
;; assembly time.  Moreover, at assembly time the position of a
;; label may vary from one call to the next due to the actions
;; of the assembler.

(define (asm-label-pos label-obj)
  (let ((pos (vector-ref label-obj 1)))
    (if pos
      pos
      (compiler-internal-error
        "asm-label-pos, undefined label" (asm-label-id label-obj)))))

;; (asm-align multiple offset) adds enough zero bytes to the code
;; stream to force alignment to the next address congruent to
;; "offset" modulo "multiple".

(define (asm-align multiple offset)
  (asm-at-assembly
    (lambda (self)
      (modulo (- multiple (- self offset)) multiple))
    (lambda (self)
      (let loop ((n (modulo (- multiple (- self offset)) multiple)))
        (if (> n 0)
          (begin
            (asm-8 0)
            (loop (- n 1))))))))

;; (asm-origin address) adds enough zero bytes to the code stream to move
;; to the address "address".

(define (asm-origin address)
  (asm-at-assembly
    (lambda (self)
      (- address self))
    (lambda (self)
      (let ((len (- address self)))
        (if (< len 0)
          (compiler-internal-error "asm-origin, can't move back")
          (let loop ((n len))
            (if (> n 0)
              (begin
                (asm-8 0)
                (loop (- n 1))))))))))

;; (asm-at-assembly . procs) makes it possible to defer code
;; production to assembly time.  A useful application is to generate
;; position dependent and span dependent code sequences.  This
;; procedure must be passed an even number of procedures.  All odd
;; indexed procedures (including the first procedure) are called "check"
;; procedures.  The even indexed procedures are the "production"
;; procedures which, when called, produce a particular code sequence.
;; A check procedure decides if, given the current state of assembly
;; (in particular the current positioning of the labels), the code
;; produced by the corresponding production procedure is valid.
;; If the code is not valid, the check procedure must return #f.
;; If the code is valid, the check procedure must return the length
;; of the code sequence in bytes.  The assembler will try each check
;; procedure in order until it finds one that does not return #f
;; (the last check procedure must never return #f).  For convenience,
;; the current position in the code sequence is passed as the single
;; argument of check and production procedures.
;;
;; Here is a sample call of "asm-at-assembly" to produce the
;; shortest branch instruction to branch to label "x" for a
;; hypothetical processor:
;;
;;  (asm-at-assembly
;;
;;    (lambda (self) ; first check procedure
;;      (let ((dist (- (asm-label-pos x) self)))
;;        (if (and (>= dist -128) (<= dist 127)) ; short branch possible?
;;          2
;;          #f)))
;;
;;    (lambda (self) ; first production procedure
;;      (asm-8 #x34) ; branch opcode for 8 bit displacement
;;      (asm-8 (- (asm-label-pos x) self)))
;;
;;    (lambda (self) 5) ; second check procedure
;;
;;    (lambda (self) ; second production procedure
;;      (asm-8 #x35) ; branch opcode for 32 bit displacement
;;      (asm-32 (- (asm-label-pos x) self))))

(define (asm-at-assembly . procs)
  (asm-code-extend (vector 'DEFERRED procs)))

;; (asm-listing text) adds text to the right side of the listing.
;; The atoms in "text" will be output using "display" (lists are
;; traversed recursively).  The listing is generated by calling
;; "asm-display-listing".

(define (asm-listing text)
  (asm-code-extend (vector 'LISTING text)))

;; (asm-assemble) assembles the code stream.  After assembly, the
;; label objects will be set to their final position and the
;; alignment bytes and the deferred code will have been produced.  It
;; is possible to extend the code stream after assembly.  However, if
;; any of the procedures "asm-label", "asm-align", and
;; "asm-at-assembly" are called, the code stream will have to be
;; assembled once more.

(define (asm-assemble)
  (let ((fixup-lst (asm-pass1)))

    (let loop1 ()
      (let loop2 ((lst fixup-lst)
                  (changed? #f)
                  (pos asm-start-pos))
        (if (null? lst)
          (if changed? (loop1))
          (let* ((fixup (car lst))
                 (pos (+ pos (car fixup)))
                 (curr (cdr fixup))
                 (x (car curr)))
            (if (eq? (vector-ref x 0) 'LABEL)
              ; LABEL
              (if (= (vector-ref x 1) pos)
                (loop2 (cdr lst) changed? pos)
                (begin
                  (vector-set! x 1 pos)
                  (loop2 (cdr lst) #t pos)))
              ; DEFERRED
              (let loop3 ()
                (let ((n ((car (vector-ref x 1)) pos)))
                  (if n
                    (loop2 (cdr lst) changed? (+ pos n))
                    (begin
                      (vector-set! x 1 (cddr (vector-ref x 1)))
                      (loop3))))))))))

    (let loop4 ((prev asm-code-stream)
                (curr (cdr asm-code-stream))
                (pos asm-start-pos))
      (if (null? curr)
        (set-car! asm-code-stream prev)
        (let ((x (car curr))
              (next (cdr curr)))
          (if (vector? x)
            (let ((kind (vector-ref x 0)))
              (cond ((eq? kind 'LABEL)
                     (let ((final-pos (vector-ref x 1)))
                       (if final-pos
                         (if (not (= pos final-pos))
                           (compiler-internal-error
                             "asm-assemble, inconsistency detected"))
                         (vector-set! x 1 pos))
                       (set-cdr! prev next)
                       (loop4 prev next pos)))
                    ((eq? kind 'DEFERRED)
                     (let ((temp asm-code-stream))
                       (set! asm-code-stream (asm-make-stream))
                       ((cadr (vector-ref x 1)) pos)
                       (let ((tail (car asm-code-stream)))
                         (set-cdr! tail next)
                         (let ((head (cdr asm-code-stream)))
                           (set-cdr! prev head)
                           (set! asm-code-stream temp)
                           (loop4 prev head pos)))))
                    (else
                     (loop4 curr next pos))))
            (loop4 curr next (+ pos 1))))))))

;; (asm-display-listing port) produces a listing of the code stream
;; on the given output port.  The bytes generated are shown in
;; hexadecimal on the left side of the listing and the right side
;; of the listing contains the text inserted by "asm-listing".

(define (asm-display-listing port)

  (define text-col 24)
  (define pos-width 6)
  (define byte-width 2)

  (define (output text)
    (cond ((null? text))
          ((pair? text)
           (output (car text))
           (output (cdr text)))
          (else
           (display text port))))

  (define (print-hex n)
    (display (string-ref "0123456789ABCDEF" n) port))

  (define (print-byte n)
    (print-hex (quotient n 16))
    (print-hex (modulo n 16)))

  (define (print-pos n)
    (if (< n 0)
      (display "      " port)
      (begin
        (print-byte (quotient n #x10000))
        (print-byte (modulo (quotient n #x100) #x100))
        (print-byte (modulo n #x100)))))

  (let loop1 ((lst (cdr asm-code-stream)) (pos asm-start-pos) (col 0))
    (if (null? lst)
      (if (> col 0)
        (newline port))
      (let ((x (car lst)))
        (if (vector? x)
          (let ((kind (vector-ref x 0)))
            (cond ((eq? kind 'LISTING)
                   (let loop2 ((col col))
                     (if (< col text-col)
                       (begin
                         (display (integer->char 9) port)
                         (loop2 (* 8 (+ (quotient col 8) 1))))))
                   (output (vector-ref x 1))
                   (newline port)
                   (loop1 (cdr lst) pos 0))
                  (else
                   (compiler-internal-error
                     "asm-display-listing, code stream not assembled"))))
          (if (or (= col 0) (>= col (- text-col byte-width)))
            (begin
              (if (not (= col 0)) (newline port))
              (print-pos pos)
              (display " " port)
              (print-byte x)
              (loop1 (cdr lst) (+ pos 1) (+ (+ pos-width 1) byte-width)))
            (begin
              (print-byte x)
              (loop1 (cdr lst) (+ pos 1) (+ col byte-width)))))))))

;; (asm-write-code filename) outputs the code stream (i.e. the sequence
;; of bytes produced) on the named file.

(define (asm-write-code filename)
  (with-output-to-file filename
    (lambda ()
      (let loop ((lst (cdr asm-code-stream)))
        (if (not (null? lst))
          (let ((x (car lst)))
            (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (if (not (eq? kind 'LISTING))
                  (compiler-internal-error
                    "asm-write-code, code stream not assembled"))
                (loop (cdr lst)))
              (begin
                (write-char (integer->char x))
                (loop (cdr lst))))))))))

(define (asm-write-hex-file filename)
  (with-output-to-file filename
    (lambda ()

      (define (print-hex n)
        (display (string-ref "0123456789ABCDEF" n)))

      (define (print-byte n)
        (print-hex (quotient n 16))
        (print-hex (modulo n 16)))

      (define (print-line type addr bytes)
        (let ((n (length bytes))
              (addr-hi (quotient addr 256))
              (addr-lo (modulo addr 256)))
          (display ":")
          (print-byte n)
          (print-byte addr-hi)
          (print-byte addr-lo)
          (print-byte type)
          (for-each print-byte bytes)
          (let ((sum
                 (modulo (- (apply + n addr-hi addr-lo type bytes)) 256)))
            (print-byte sum)
            (newline))))

      (let loop ((lst (cdr asm-code-stream))
                 (pos asm-start-pos)
                 (rev-bytes '()))
        (if (not (null? lst))
          (let ((x (car lst)))
            (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (if (not (eq? kind 'LISTING))
                  (compiler-internal-error
                    "asm-write-hex-file, code stream not assembled"))
                (loop (cdr lst)
                      pos
                      rev-bytes))
              (let ((new-pos
                     (+ pos 1))
                    (new-rev-bytes
                     (cons x
                           (if (= (modulo pos 16) 0)
                               (begin
                                 (print-line 0
                                             (- pos (length rev-bytes))
                                             (reverse rev-bytes))
                                 '())
                               rev-bytes))))
                (loop (cdr lst)
                      new-pos
                      new-rev-bytes))))
          (begin
            (if (not (null? rev-bytes))
                (print-line 0
                            (- pos (length rev-bytes))
                            (reverse rev-bytes)))
            (print-line 1 0 '())
            (if #t
                (begin
                  (display (- pos asm-start-pos) ##stderr-port)
                  (display " bytes\n" ##stderr-port)))))))))

;; Utilities.

(define asm-start-pos #f)   ; start position of the code stream
(define asm-big-endian? #f) ; endianness to use
(define asm-code-stream #f) ; current code stream

(define (asm-make-stream) ; create an empty stream
  (let ((x (cons '() '())))
    (set-car! x x)
    x))
     
(define (asm-code-extend item) ; add an item at the end of current code stream
  (let* ((stream asm-code-stream)
         (tail (car stream))
         (cell (cons item '())))
    (set-cdr! tail cell)
    (set-car! stream cell)))

(define (asm-pass1) ; construct fixup list and make first label assignment
  (let loop ((curr (cdr asm-code-stream))
             (fixup-lst '())
             (span 0)
             (pos asm-start-pos))
    (if (null? curr)
      (reverse fixup-lst)
      (let ((x (car curr)))
        (if (vector? x)
          (let ((kind (vector-ref x 0)))
            (cond ((eq? kind 'LABEL)
                   (vector-set! x 1 pos) ; first approximation of position
                   (loop (cdr curr) (cons (cons span curr) fixup-lst) 0 pos))
                  ((eq? kind 'DEFERRED)
                   (loop (cdr curr) (cons (cons span curr) fixup-lst) 0 pos))
                  (else
                   (loop (cdr curr) fixup-lst span pos))))
          (loop (cdr curr) fixup-lst (+ span 1) (+ pos 1)))))))

;(##declare (generic))

(define (asm-bits-0-to-7 n) ; return bits 0 to 7 of a signed integer
  (modulo n #x100))

(define (asm-bits-8-and-up n) ; return bits 8 and up of a signed integer
  (if (>= n 0)
    (quotient n #x100)
    (- (quotient (+ n 1) #x100) 1)))

(define (asm-bits-16-and-up n) ; return bits 16 and up of a signed integer
  (if (>= n 0)
    (quotient n #x10000)
    (- (quotient (+ n 1) #x10000) 1)))

(define (asm-bits-32-and-up n) ; return bits 32 and up of a signed integer
  (if (>= n 0)
    (quotient n #x100000000)
    (- (quotient (+ n 1) #x100000000) 1)))

; The following procedures convert floating point numbers into their
; machine representation.  They perform bignum and flonum arithmetic.

(define (asm-float->inexact-exponential-format x)

  (define (exp-form-pos x y i)
    (let ((i*2 (+ i i)))
      (let ((z (if (and (not (< asm-ieee-e-bias i*2))
                        (not (< x y)))
                 (exp-form-pos x (* y y) i*2)
                 (cons x 0))))
        (let ((a (car z)) (b (cdr z)))
          (let ((i+b (+ i b)))
            (if (and (not (< asm-ieee-e-bias i+b))
                     (not (< a y)))
              (begin
                (set-car! z (/ a y))
                (set-cdr! z i+b)))
            z)))))

  (define (exp-form-neg x y i)
    (let ((i*2 (+ i i)))
      (let ((z (if (and (< i*2 asm-ieee-e-bias-minus-1)
                        (< x y))
                 (exp-form-neg x (* y y) i*2)
                 (cons x 0))))
        (let ((a (car z)) (b (cdr z)))
          (let ((i+b (+ i b)))
            (if (and (< i+b asm-ieee-e-bias-minus-1)
                     (< a y))
              (begin
                (set-car! z (/ a y))
                (set-cdr! z i+b)))
            z)))))

  (define (exp-form x)
    (if (< x asm-inexact-+1)
      (let ((z (exp-form-neg x asm-inexact-+1/2 1)))
        (set-car! z (* asm-inexact-+2 (car z)))
        (set-cdr! z (- -1 (cdr z)))
        z)
      (exp-form-pos x asm-inexact-+2 1)))

  (if (negative? x)
    (let ((z (exp-form (- asm-inexact-0 x))))
      (set-car! z (- asm-inexact-0 (car z)))
      z)
    (exp-form x)))

(define (asm-float->exact-exponential-format x)
  (let ((z (asm-float->inexact-exponential-format x)))
    (let ((y (car z)))
      (cond ((not (< y asm-inexact-+2))
             (set-car! z asm-ieee-+m-min)
             (set-cdr! z asm-ieee-e-bias-plus-1))
            ((not (< asm-inexact--2 y))
             (set-car! z asm-ieee--m-min)
             (set-cdr! z asm-ieee-e-bias-plus-1))
            (else
             (set-car! z
               (truncate (inexact->exact (* (car z) asm-inexact-m-min))))))
      (set-cdr! z (- (cdr z) asm-ieee-m-bits))
      z)))

(define (asm-float->bits x) ; returns the 64 bit integer encoding the float "x"

  (define (bits a b)
    (if (< a asm-ieee-+m-min)
      a
      (+ (- a asm-ieee-+m-min)
         (* (+ (+ b asm-ieee-m-bits) asm-ieee-e-bias)
            asm-ieee-+m-min))))

  (let ((z (asm-float->exact-exponential-format x)))
    (let ((a (car z)) (b (cdr z)))
      (if (negative? a)
        (+ asm-ieee-sign-bit (bits (- 0 a) b))
        (bits a b)))))

; Parameters for ANSI-IEEE Std 754-1985 representation of
; doubles (i.e. 64 bit floating point numbers):

(define asm-ieee-m-bits 52)
(define asm-ieee-e-bits 11)
(define asm-ieee-+m-min 4503599627370496)    ; (expt 2 asm-ieee-m-bits)
(define asm-ieee--m-min -4503599627370496)   ; (- asm-ieee-+m-min)
(define asm-ieee-sign-bit #x8000000000000000); (expt 2 (+ asm-ieee-e-bits asm-ieee-m-bits))

(define asm-ieee-e-bias         1023) ; (- (expt 2 (- asm-ieee-e-bits 1)) 1)
(define asm-ieee-e-bias-plus-1  1024) ; (+ asm-ieee-e-bias 1)
(define asm-ieee-e-bias-minus-1 1022) ; (- asm-ieee-e-bias 1)

(define asm-inexact-m-min (exact->inexact asm-ieee-+m-min))
(define asm-inexact-+2    (exact->inexact 2))
(define asm-inexact--2    (exact->inexact -2))
(define asm-inexact-+1    (exact->inexact 1))
(define asm-inexact-+1/2  (exact->inexact (/ 1 2)))
(define asm-inexact-0     (exact->inexact 0))

;------------------------------------------------------------------------------

(define min-fixnum-encoding 3)
(define min-fixnum -1)
(define max-fixnum 255)
(define min-rom-encoding (+ min-fixnum-encoding (- max-fixnum min-fixnum) 1))
(define min-ram-encoding 512)
(define max-ram-encoding 4095)
(define min-vec-encoding 4096)
(define max-vec-encoding 8191)

(define code-start #x5000)

(define (predef-constants) (list))

(define (predef-globals) (list))

(define (encode-direct obj)
  (cond ((eq? obj #f)
         0)
        ((eq? obj #t)
         1)
        ((eq? obj '())
         2)
        ((and (integer? obj)
              (exact? obj)
              (>= obj min-fixnum)
              (<= obj max-fixnum))
         (+ obj (- min-fixnum-encoding min-fixnum)))
        (else
         #f)))

(define (translate-constant obj)
  (if (char? obj)
      (char->integer obj)
      obj))

(define (encode-constant obj constants)
  (let ((o (translate-constant obj)))
    (let ((e (encode-direct o)))
      (if e
          e
          (let ((x (assoc o constants)))
            (if x
                (vector-ref (cdr x) 0)
                (compiler-error "unknown object" obj)))))))

;; TODO actually, seem to be in a pair, scheme object in car, vector in cdr
;; constant objects are represented by vectors
;; 0 : encoding (ROM address) TODO really the ROM address ?
;; 1 : TODO asm label constant ?
;; 2 : number of occurences of this constant in the code
;; 3 : pointer to content, used at encoding time
(define (add-constant obj constants from-code? cont)
  (let ((o (translate-constant obj)))
    (let ((e (encode-direct o)))
      (if e
          (cont constants)
          (let ((x (assoc o constants)))
            (if x
                (begin
                  (if from-code?
                      (vector-set! (cdr x) 2 (+ (vector-ref (cdr x) 2) 1)))
                  (cont constants))
                (let* ((descr
                        (vector #f
                                (asm-make-label 'constant)
                                (if from-code? 1 0)
                                #f))
                       (new-constants
                        (cons (cons o descr)
                              constants)))
                  (cond ((pair? o)
                         (add-constants (list (car o) (cdr o))
                                        new-constants
                                        cont))
                        ((symbol? o)
                         (cont new-constants))
                        ((string? o)
                         (let ((chars (map char->integer (string->list o))))
                           (vector-set! descr 3 chars)
                           (add-constant chars
                                         new-constants
                                         #f
                                         cont)))
                        ((vector? o) ; ordinary vectors are stored as lists
                         (let ((elems (vector->list o)))
                           (vector-set! descr 3 elems)
                           (add-constant elems
                                         new-constants
                                         #f
                                         cont)))
			((u8vector? o)			 
			 (let ((elems (u8vector->list o)))
			   (vector-set! descr 3 elems)
			   (add-constant elems
					 new-constants
					 #f
					 cont)))
			((and (number? o) (exact? o))
			 ; (pp (list START-ENCODING: o))
			 (let loop1 ((n o)
				     (acc '()))
			   ;; acc will be the list of the linked blocks,
			   ;; most significant part first
			   ;; TODO use CPS, like everyone else
			   ; (pp (list N: n HI: (arithmetic-shift n -16) LO: (modulo n (expt 2 16))))
			   (if (not (or (= n 0) (= n -1)))
			       (loop1 (arithmetic-shift n -16)
				      (cons (modulo n (expt 2 16))
					    acc))
			       (let loop2 ((acc           acc)
					   (prev          n) ; 0 or -1
					   (descr         descr)
					   (new-constants new-constants))
				 ; (pp (list LOOP2: acc: acc prev: prev descr: descr))
				 (if (null? acc) ; everything was encoded
				     (cont new-constants)
				     (begin
				       (vector-set! descr 3 prev) ; value of hi
				       (let ((descr ;; TODO OOPS nowhere is the actual value...
					      (vector ;; TODO same as previous, asbtract ?
					       #f
					       (asm-make-label 'constant)
					       (if from-code? 1 0)
					       #f))
					     (new-val
					      ;; numerical value of the object
					      (+ (arithmetic-shift prev 16)
						 (car acc))))
					 (loop2 (cdr acc)
						new-val
						descr
						;; TODO find a way to prevent having 2 objects with the
						;; same value, to force sharing
						;; right now, I'm not sure sharing is done
						;; perhaps, since, in the sorted list, if we search for
						;; a certain value, only the 1st of it will show up
						(cons (cons new-val descr)
						      constants))))))))) ;; TODO FOOBIGNUMS
                        (else
                         (cont new-constants))))))))))

(define (add-constants objs constants cont)
  (if (null? objs)
      (cont constants)
      (add-constant (car objs)
                    constants
                    #f
                    (lambda (new-constants)
                      (add-constants (cdr objs)
                                     new-constants
                                     cont)))))

(define (add-global var globals cont)
  (let ((x (assq var globals)))
    (if x	
        (begin
	  ;; increment reference counter
	  (vector-set! (cdr x) 1 (+ (vector-ref (cdr x) 1) 1))
	  (cont globals))
        (let ((new-globals
               (cons (cons var (vector (length globals) 1))
                     globals)))
	  (cont new-globals)))))

(define (sort-constants constants)
  (let ((csts
         (sort-list constants
                    (lambda (x y)
                      (> (vector-ref (cdr x) 2)
                         (vector-ref (cdr y) 2))))))
    (let loop ((i min-rom-encoding)
               (lst csts))
      (if (null? lst)
	  ;; constants can use all the rom addresses up to 256 constants since
	  ;; their number is encoded in a byte at the beginning of the bytecode
          (if (or (> i min-ram-encoding) (> (- i min-rom-encoding) 256))
	      (compiler-error "too many constants")
	      csts)
          (begin
            (vector-set! (cdr (car lst)) 0 i)
            (loop (+ i 1)
                  (cdr lst)))))))

(define (sort-globals globals) ;; TODO a lot in common with sort-constants, ABSTRACT
  (let ((glbs
	 (sort-list globals
		    (lambda (x y)
		      (> (vector-ref (cdr x) 1)
			 (vector-ref (cdr y) 1))))))
    (let loop ((i 0)
	       (lst glbs))
      (if (null? lst)
	  (if (> i 256) ;; the number of globals is encoded on a byte
	      (compiler-error "too many global variables")
	      glbs)	  
	  (begin
	    (vector-set! (cdr (car lst)) 0 i)
	    (loop (+ i 1)
		  (cdr lst)))))))

(define assemble
  (lambda (code hex-filename)
    (let loop1 ((lst code)
                (constants (predef-constants))
                (globals (predef-globals))
                (labels (list)))
      (if (pair? lst)

          (let ((instr (car lst)))
            (cond ((number? instr)
                   (loop1 (cdr lst)
                          constants
                          globals
                          (cons (cons instr (asm-make-label 'label))
                                labels)))
                  ((eq? (car instr) 'push-constant)
                   (add-constant (cadr instr)
                                 constants
                                 #t
                                 (lambda (new-constants)
                                   (loop1 (cdr lst)
                                          new-constants
                                          globals
                                          labels))))
                  ((memq (car instr) '(push-global set-global))
                   (add-global (cadr instr)
                               globals
                               (lambda (new-globals)
                                 (loop1 (cdr lst)
                                        constants
                                        new-globals
                                        labels))))
                  (else
                   (loop1 (cdr lst)
                          constants
                          globals
                          labels))))

          (let ((constants (sort-constants constants))
 		(globals   (sort-globals   globals)))

            (define (label-instr label opcode)
              (asm-at-assembly
	       ;; if the distance from pc to the label fits in a single byte,
	       ;; a short instruction is used, containing a relative address
	       ;; if not, the full 16-bit label is used
;;; 	       (lambda (self)
;;; 		 (let ((dist (- (asm-label-pos label) self)))
;;; 		   (and (< dist 256) ;; TODO have this between -128 and 127 ? would be more flexible, I guess
;;; 			(> dist 0)
;;; 			2)))
;;; 	       (lambda (self)
;;; 		 (asm-8 (+ opcode 5))
;;; 		 (asm-8 (- (asm-label-pos label) self)))
	       ;; TODO doesn't work at the moment
	       
               (lambda (self)
		 3)
               (lambda (self)
		 (let ((pos (- (asm-label-pos label) code-start)))
			 (asm-8 opcode)
			 (asm-8 (quotient pos 256))
			 (asm-8 (modulo pos 256))))))

            (define (push-constant n)
              (if (<= n 31)
                  (asm-8 (+ #x00 n))
                  (begin
                    (asm-8 (+ #x90 (quotient n 256)))
		    (asm-8 (modulo n 256)))))

            (define (push-stack n)
              (if (> n 31)
                  (compiler-error "stack is too deep")
                  (asm-8 (+ #x20 n))))

            (define (push-global n)
	      (if (<= n 15)
		  (asm-8 (+ #x40 n))
		  (begin (asm-8 #x8e)
			 (asm-8 n))))

            (define (set-global n)
              (if (<= n 15)
	          (asm-8 (+ #x50 n))
		  (begin (asm-8 #x8f)
			 (asm-8 n))))

            (define (call n)
              (if (> n 15)
	          (compiler-error "call has too many arguments")
	          (asm-8 (+ #x60 n))))

            (define (jump n)
              (if (> n 15)
                  (compiler-error "call has too many arguments")
                  (asm-8 (+ #x70 n))))

            (define (call-toplevel label)
              (label-instr label #x80))

            (define (jump-toplevel label)
              (label-instr label #x81))

            (define (goto label)
              (label-instr label #x82))

            (define (goto-if-false label)
              (label-instr label #x83))

            (define (closure label)
              (label-instr label #x84))

            (define (prim n)
              (asm-8 (+ #xc0 n)))

            (define (prim.number?)         (prim 0))
            (define (prim.+)               (prim 1))
            (define (prim.-)               (prim 2))
            (define (prim.*)               (prim 3))
            (define (prim.quotient)        (prim 4))
            (define (prim.remainder)       (prim 5))
            (define (prim.neg)             (prim 6))
            (define (prim.=)               (prim 7))
            (define (prim.<)               (prim 8))
	    (define (prim.ior)             (prim 9))
            (define (prim.>)               (prim 10))
	    (define (prim.xor)             (prim 11))
            (define (prim.pair?)           (prim 12))
            (define (prim.cons)            (prim 13))
            (define (prim.car)             (prim 14))
            (define (prim.cdr)             (prim 15))
            (define (prim.set-car!)        (prim 16))
            (define (prim.set-cdr!)        (prim 17))
            (define (prim.null?)           (prim 18))
            (define (prim.eq?)             (prim 19))
            (define (prim.not)             (prim 20))
            (define (prim.get-cont)        (prim 21))
            (define (prim.graft-to-cont)   (prim 22))
            (define (prim.return-to-cont)  (prim 23))
            (define (prim.halt)            (prim 24))
            (define (prim.symbol?)         (prim 25))
            (define (prim.string?)         (prim 26))
            (define (prim.string->list)    (prim 27))
            (define (prim.list->string)    (prim 28))
	    (define (prim.make-u8vector)   (prim 29))
	    (define (prim.u8vector-ref)    (prim 30))
	    (define (prim.u8vector-set!)   (prim 31))
            (define (prim.print)           (prim 32))
            (define (prim.clock)           (prim 33))
            (define (prim.motor)           (prim 34))
            (define (prim.led)             (prim 35))
	    (define (prim.led2-color)      (prim 36))
	    (define (prim.getchar-wait)    (prim 37))
	    (define (prim.putchar)         (prim 38))
	    (define (prim.beep)            (prim 39))
	    (define (prim.adc)             (prim 40))
	    (define (prim.u8vector?)       (prim 41))
	    (define (prim.sernum)          (prim 42))
	    (define (prim.u8vector-length) (prim 43))
	    (define (prim.u8vector-copy!)  (prim 44))
            (define (prim.shift)           (prim 45))
            (define (prim.pop)             (prim 46))
            (define (prim.return)          (prim 47))
	    (define (prim.boolean?)        (prim 48))
	    (define (prim.network-init)    (prim 49))
	    (define (prim.network-cleanup) (prim 50))
	    (define (prim.receive-packet-to-u8vector) (prim 51))
	    (define (prim.send-packet-from-u8vector)  (prim 52))
	    
            (define big-endian? #f)

            (asm-begin! code-start #f)

            (asm-8 #xfb)
            (asm-8 #xd7)
            (asm-8 (length constants))
            (asm-8 (length globals))

            '(pp (list constants: constants globals: globals))

            (for-each
             (lambda (x)
               (let* ((descr (cdr x))
                      (label (vector-ref descr 1))
                      (obj (car x)))
                 (asm-label label)
		 ;; see the vm source for a description of encodings
		 ;; TODO have comments here to explain encoding, at least magic number that give the type
                 (cond ((and (integer? obj) (exact? obj)) ;; TODO FOOBGIGNUMS
			(let ((hi (encode-constant (vector-ref descr 3)
						   constants)))
			  (pp (list ENCODE: (vector-ref descr 3) to: hi lo: obj))
			  (asm-8 (+ 0 (arithmetic-shift hi -8)))
			  (asm-8 (bitwise-and hi  #xff)) ; pointer to hi
			  (asm-8 (bitwise-and obj #xff00)) ; bits 8-15
			  (asm-8 (bitwise-and obj #xff)))) ; bits 0-7
                       ((pair? obj)
			(let ((obj-car (encode-constant (car obj) constants))
			      (obj-cdr (encode-constant (cdr obj) constants)))
			  (asm-8 (+ #x80 (arithmetic-shift obj-car -8)))
			  (asm-8 (bitwise-and obj-car #xff))
			  (asm-8 (+ 0 (arithmetic-shift obj-cdr -8)))
			  (asm-8 (bitwise-and obj-cdr #xff))))
                       ((symbol? obj)
                        (asm-8 #x80)
                        (asm-8 0)
                        (asm-8 #x20)
                        (asm-8 0))
                       ((string? obj)
			(let ((obj-enc (encode-constant (vector-ref descr 3)
							constants)))
			  (asm-8 (+ #x80 (arithmetic-shift obj-enc -8)))
			  (asm-8 (bitwise-and obj-enc #xff))
			  (asm-8 #x40)
			  (asm-8 0)))
                       ((vector? obj) ; ordinary vectors are stored as lists
			(let* ((elems (vector-ref descr 3))
			       (obj-car (encode-constant (car elems)
							 constants))
			       (obj-cdr (encode-constant (cdr elems)
							 constants)))
			  (asm-8 (+ #x80 (arithmetic-shift obj-car -8)))
			  (asm-8 (bitwise-and obj-car #xff))
			  (asm-8 (+ 0 (arithmetic-shift obj-cdr -8)))
			  (asm-8 (bitwise-and obj-cdr #xff))))
		       ((u8vector? obj)
			(let ((obj-enc (encode-constant (vector-ref descr 3)
							constants))
			      (l (length (vector-ref descr 3))))
			  ;; length is stored raw, not encoded as an object
			  ;; however, the bytes of content are encoded as
			  ;; fixnums
			  (asm-8 (+ #x80 (arithmetic-shift l -8)))
			  (asm-8 (bitwise-and l #xff))
			  (asm-8 (+ #x60 (arithmetic-shift obj-enc -8)))
			  (asm-8 (bitwise-and obj-enc #xff))))
                       (else
                        (compiler-error "unknown object type" obj)))))
             constants)

            (let loop2 ((lst code))
              (if (pair? lst)
                  (let ((instr (car lst)))

                    (cond ((number? instr)
                           (let ((label (cdr (assq instr labels))))
                             (asm-label label)))

                          ((eq? (car instr) 'entry)
                           (let ((np (cadr instr))
                                 (rest? (caddr instr)))
                             (asm-8 (if rest? (- np) np))))

                          ((eq? (car instr) 'push-constant)
                           (let ((n (encode-constant (cadr instr) constants)))
                             (push-constant n)))

                          ((eq? (car instr) 'push-stack)
                           (push-stack (cadr instr)))

                          ((eq? (car instr) 'push-global)
                           (push-global (vector-ref
					 (cdr (assq (cadr instr) globals))
					 0)))

                          ((eq? (car instr) 'set-global)
			   (set-global (vector-ref
					(cdr (assq (cadr instr) globals))
					0)))

                          ((eq? (car instr) 'call)
                           (call (cadr instr)))

                          ((eq? (car instr) 'jump)
                           (jump (cadr instr)))

                          ((eq? (car instr) 'call-toplevel)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (call-toplevel label)))

                          ((eq? (car instr) 'jump-toplevel)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (jump-toplevel label)))

                          ((eq? (car instr) 'goto)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (goto label)))

                          ((eq? (car instr) 'goto-if-false)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (goto-if-false label)))

                          ((eq? (car instr) 'closure)
                           (let ((label (cdr (assq (cadr instr) labels))))
                             (closure label)))

                          ((eq? (car instr) 'prim)
                           (case (cadr instr)
                             ((#%number?)         (prim.number?))
                             ((#%+)               (prim.+))
                             ((#%-)               (prim.-))
                             ((#%*)               (prim.*))
                             ((#%quotient)        (prim.quotient))
                             ((#%remainder)       (prim.remainder))
                             ((#%neg)             (prim.neg))
                             ((#%=)               (prim.=))
                             ((#%<)               (prim.<))
			     ((#%ior)             (prim.ior))
                             ((#%>)               (prim.>))
			     ((#%xor)             (prim.xor))
                             ((#%pair?)           (prim.pair?))
                             ((#%cons)            (prim.cons))
                             ((#%car)             (prim.car))
                             ((#%cdr)             (prim.cdr))
                             ((#%set-car!)        (prim.set-car!))
                             ((#%set-cdr!)        (prim.set-cdr!))
                             ((#%null?)           (prim.null?))
                             ((#%eq?)             (prim.eq?))
                             ((#%not)             (prim.not))
                             ((#%get-cont)        (prim.get-cont))
                             ((#%graft-to-cont)   (prim.graft-to-cont))
                             ((#%return-to-cont)  (prim.return-to-cont))
                             ((#%halt)            (prim.halt))
                             ((#%symbol?)         (prim.symbol?))
                             ((#%string?)         (prim.string?))
                             ((#%string->list)    (prim.string->list))
                             ((#%list->string)    (prim.list->string))
			     ((#%make-u8vector)   (prim.make-u8vector))
			     ((#%u8vector-ref)    (prim.u8vector-ref))
			     ((#%u8vector-set!)   (prim.u8vector-set!))
                             ((#%print)           (prim.print))
                             ((#%clock)           (prim.clock))
                             ((#%motor)           (prim.motor))
                             ((#%led)             (prim.led))
			     ((#%led2-color)      (prim.led2-color))
                             ((#%getchar-wait )   (prim.getchar-wait))
                             ((#%putchar)         (prim.putchar))
			     ((#%beep)            (prim.beep))
                             ((#%adc)             (prim.adc))
                             ((#%u8vector?)       (prim.u8vector?))
                             ((#%sernum)          (prim.sernum))
			     ((#%u8vector-length) (prim.u8vector-length))
			     ((#%u8vector-copy!)  (prim.u8vector-copy!))
			     ((#%boolean?)        (prim.boolean?))
			     ((#%network-init)    (prim.network-init))
			     ((#%network-cleanup) (prim.network-cleanup))
			     ((#%receive-packet-to-u8vector) (prim.receive-packet-to-u8vector))
			     ((#%send-packet-from-u8vector)  (prim.send-packet-from-u8vector))
                             (else
                              (compiler-error "unknown primitive" (cadr instr)))))

                          ((eq? (car instr) 'return)
                           (prim.return))

                          ((eq? (car instr) 'pop)
                           (prim.pop))

                          ((eq? (car instr) 'shift)
                           (prim.shift))

                          (else
                           (compiler-error "unknown instruction" instr)))

                    (loop2 (cdr lst)))))

            (asm-assemble)

            (asm-write-hex-file hex-filename)

            (asm-end!))))))

(define execute
  (lambda (hex-filename)
'
    (if #f
        (begin
          (shell-command "gcc -o picobit-vm picobit-vm.c")
          (shell-command (string-append "./picobit-vm " hex-filename)))
        (shell-command (string-append "./robot . 1 " hex-filename)))))

(define (sort-list l <?)

  (define (mergesort l)

    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (<? e1 e2)
                 (cons e1 (merge (cdr l1) l2))
                 (cons e2 (merge l1 (cdr l2))))))))

    (define (split l)
      (if (or (null? l) (null? (cdr l)))
        l
        (cons (car l) (split (cddr l)))))

    (if (or (null? l) (null? (cdr l)))
      l
      (let* ((l1 (mergesort (split l)))
             (l2 (mergesort (split (cdr l)))))
        (merge l1 l2))))

  (mergesort l))

;------------------------------------------------------------------------------

(define compile
  (lambda (filename)
    (let* ((node (parse-file filename))
           (hex-filename
            (string-append
             (path-strip-extension filename)
             ".hex")))
      
      (adjust-unmutable-references! node)

;      (pp (node->expr node))

      (let ((ctx (comp-none node (make-init-context))))
        (let ((prog (linearize (optimize-code (context-code ctx)))))
;         (pp (list code: prog env: (context-env ctx)))
	  (assemble prog hex-filename)
	  (execute hex-filename))))))


(define main
  (lambda (filename)
    (compile filename)))

;------------------------------------------------------------------------------

'
(define (asm-write-hex-file filename)
  (with-output-to-file filename
    (lambda ()

      (define (print-hex n)
        (display (string-ref "0123456789ABCDEF" n)))

      (define (print-byte n)
        (display ", 0x")
        (print-hex (quotient n 16))
        (print-hex (modulo n 16)))

      (define (print-line type addr bytes)
        (let ((n (length bytes))
              (addr-hi (quotient addr 256))
              (addr-lo (modulo addr 256)))
;          (display ":")
;          (print-byte n)
;          (print-byte addr-hi)
;          (print-byte addr-lo)
;          (print-byte type)
          (for-each print-byte bytes)
          (let ((sum
                 (modulo (- (apply + n addr-hi addr-lo type bytes)) 256)))
;            (print-byte sum)
            (newline))))

      (let loop ((lst (cdr asm-code-stream))
                 (pos asm-start-pos)
                 (rev-bytes '()))
        (if (not (null? lst))
          (let ((x (car lst)))
            (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (if (not (eq? kind 'LISTING))
                  (compiler-internal-error
                    "asm-write-hex-file, code stream not assembled"))
                (loop (cdr lst)
                      pos
                      rev-bytes))
              (let ((new-pos
                     (+ pos 1))
                    (new-rev-bytes
                     (cons x
                           (if (= (modulo pos 8) 0)
                               (begin
                                 (print-line 0
                                             (- pos (length rev-bytes))
                                             (reverse rev-bytes))
                                 '())
                               rev-bytes))))
                (loop (cdr lst)
                      new-pos
                      new-rev-bytes))))
          (begin
            (if (not (null? rev-bytes))
                (print-line 0
                            (- pos (length rev-bytes))
                            (reverse rev-bytes)))
            (print-line 1 0 '())))))))

#lang racket

;;;; File: "parser.scm", Time-stamp: <2006-05-08 16:04:37 feeley>

;;;; Copyright (C) 2004-2009 by Marc Feeley and Vincent St-Amour
;;;; All Rights Reserved.

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
		(assoc (car expr) '((+ . #%+) (- . #%-))))
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

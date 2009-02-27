;;;; File: "node.scm", Time-stamp: <2006-05-08 16:04:37 feeley>

;;;; Copyright (C) 2004-2009 by Marc Feeley and Vincent St-Amour
;;;; All Rights Reserved.

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

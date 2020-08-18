#lang racket

;; Export functions
(provide evaluate
         to-string
         help)

;; Make namespace for eval
(define-namespace-anchor na)
(define NS (namespace-anchor->namespace na))

;;;
;;; Below define functions that can be called in web console
;;;
(define (help)
  `(ul
    (li "Available commands :")
    (li (span ((class "help-cmd")) "(help)") (span "Show help message"))))

;;;
;;; Evaluate the expression with namespace of current module.
;;;
;;; If success, return the expression value.
;;; If error, return the error message.
(define (evaluate expr)
  (with-handlers ([exn? exn-message])
    (eval (with-input-from-string expr read) NS)))

;; Convert value to string 
(define (to-string val)
  (cond
    ((string? val) val)
    ((list? val)   val)
    (else (with-output-to-string
            (λ () (print val))))))
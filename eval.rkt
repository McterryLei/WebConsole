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
  (define (render-help cmd desc)
    `(li (span ((class "help-cmd")) ,cmd) (spac ,desc)))

  (html-tag `(ul
              (li "Available commands :")
              ,(render-help "(help)" "Show help message"))))

;;;
;;; Evaluate the expression with namespace of current module.
;;;
;;; If success, return the expression value.
;;; If error, return the error message.
(define (evaluate expr)
  (with-handlers ([exn? exn-message])
    (eval (with-input-from-string expr read) NS)))

(define (html-tag item)
  (cons 'html item))

(define (html-tag? item)
  (if (list? item)
      (eq? 'html (car item))
      false))

;; Convert value to string 
(define (to-string val)
  (cond
    ((string? val) val)
    ((html-tag? val) (cdr val))
    (else (with-output-to-string
            (λ () (print val))))))
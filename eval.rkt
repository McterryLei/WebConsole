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
  (define commands
    (list (cons "(help)" "Show help message")))
                         
  (define (render-line item)
    `(li (span ((class "help-cmd")) ,(car item))
         (span ,(cdr item))))

  (html-tag `(ul
              (li "Available commands :")
              ,@(map render-line commands))))

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
    ((html-tag? val) (cdr val))
    (else (with-output-to-string
            (λ () (print val))))))

;; Add html tag to item
(define (html-tag item)
  (cons 'html item))

;; Check if contains html tag
(define (html-tag? item)
  (if (list? item)
      (eq? 'html (car item))
      false))
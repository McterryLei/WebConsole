#lang racket

;; Export functions
(provide evaluate
         to-string)

;; Make namespace for eval
(define NS (make-base-namespace))

;;;
;;; Evaluate the expression.
;;;
;;; If success, return the expression value.
;;; If error, return the error message.
(define (evaluate expr)
  (with-handlers ([exn? exn-message])
    (eval (with-input-from-string expr read) NS)))

;; Convert value to string 
(define (to-string val)
  (if (string? val)
      val
      (with-output-to-string
        (λ () (print val)))))
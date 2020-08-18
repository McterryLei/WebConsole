#lang web-server/insta

(define NS (make-base-namespace))

(struct history (records) #:mutable)

(struct record (command result))

(define (history-append! a-history a-record)
  (set-history-records! a-history
                        (append (history-records a-history) (list a-record))))

(define (history-dump a-history)
  (printf "\nHistory:\n")
  (for ([each (history-records a-history)])
    (printf "~a -> ~a\n" (record-command each) (record-result each))))

(static-files-path "Resources")

(define (start request)
  (render-console-page (history null) request))

(define (render-console-page a-history request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head
        (meta ((charset "utf-8")))
        (title "Scheme Console")
        (link ((href "/style.css") (rel "stylesheet") (type "text/css")))
        (script ((src "/jquery-1.7.2.min.js")))
        (script
           "$(document).ready(function() {
              $(\"#in\")[0].focus();
            });")
        (body
         (div ((class "title"))
              (span "Scheme Console"))
         (div ((id "text"))
              ,(render-history a-history)
              (br)
              (form ((action
                      ,(embed/url execute-command-handler)))
                     "Please input command:"
                    (input ((id "in")
                            (type "text")
                            (name "command"))))))))))

  (define (execute-command-handler request)
    (let* ([command (extract-binding/single 'command
                                            (request-bindings request))]
           [result (evaluate command)])
      (history-append! a-history (record command result))
      (render-console-page a-history request)))
  
  (send/suspend/dispatch response-generator))

(define (render-history a-history)
  `(ul ,@(map render-record (history-records a-history))))

(define (render-record a-record)
  (let ([command (record-command a-record)]
        [result  (record-result a-record)])
    `(ul
      (li ,(string-append "> " command))
      (li ,(to-string result)))))

(define (evaluate command)
  (with-handlers ([exn? exn-message])
    (eval (with-input-from-string command read) NS)))

(define (to-string val)
  (cond
    ((string? val) val)
    ((number? val) (number->string val))
    ((symbol? val) (symbol->string val))
    (else "ERROR: Unknown value")))
       
  



   

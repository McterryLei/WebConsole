#lang racket
 
(require web-server/servlet)
(provide/contract (start (request? . -> . response?)))

(require "model.rkt")
(require "eval.rkt")

;; Web entry
(define (start request)
  (render-console-page (history null) request))

;; Render the console with command history
(define (render-console-page a-history request)
  ;; Implement of rendering console page
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
              (span "Scheme Console")
              (a ((id "clear-btn" )
                  (href ,(embed/url clear-handler)))
                 "Clear"))
         (div ((id "text"))
              (div ((class "help"))
                   ,(to-string (help)))
              (br)
              ,(render-history a-history)
              (br)
              (form ((action
                      ,(embed/url execute-command-handler)))
                     "Please input command:"
                    (input ((id "in")
                            (type "text")
                            (name "command"))))))))))

  ;; Handler for execute a new command 
  (define (execute-command-handler request)
    (let* ([command (extract-binding/single 'command
                                            (request-bindings request))]
           [result (evaluate command)])
      (history-append! a-history (record command result))
      (render-console-page a-history (redirect/get))))

  ;; Handler for clear button
  (define (clear-handler request)
    (render-console-page (history null) request))
  
  (send/suspend/dispatch response-generator))

;; Render the command history 
(define (render-history a-history)
  `(ul ,@(map render-record (history-records a-history))))

;; Render a record
(define (render-record a-record)
  (let ([command (record-command a-record)]
        [result  (record-result a-record)])
    `(ul
      (li ,(string-append "> " command))
      (li ,(to-string result)))))

;; Servlet config
(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #t
               #:quit? #f
               #:listen-ip #f
               #:port 80
               #:extra-files-paths
               (list (build-path (current-directory) "Resources"))
               #:server-root-path "/index.rkt"
               #:servlet-path "/index.rkt")
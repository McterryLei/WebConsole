#lang racket

(provide (all-defined-out))

;; History of all commands
(struct history (records) #:mutable)

;; A history record consists of command and its result
(struct record (command result))

;; Append new record to history
(define (history-append! a-history a-record)
  (set-history-records! a-history
                        (append (history-records a-history) (list a-record))))

;; Print out all records in history
(define (history-dump a-history)
  (printf "\nHistory:\n")
  (for ([each (history-records a-history)])
    (printf "~a -> ~a\n" (record-command each) (record-result each))))
#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require ragg/support)
(require "shredd-parse.ragg.rkt")
(require syntax/strip-context)

(define-syntax-rule (tokenizer ip patterns ...)
  (begin
    (port-count-lines! ip)
    (define my-lexer
      (lexer-src-pos patterns ...))
    (define (next-token) (my-lexer ip))
    next-token))

(define (tokenize ip)
  (tokenizer ip
      [(:+ whitespace)
       (token 'WHITESPACE lexeme #:skip? #t)]
      [#\=
       (token 'EQUALS lexeme)]
      [#\;
       (token 'SEMI lexeme)]
      [(:+ (:& any-char
	     (complement
	       (:or #\= #\;))))
       (token 'PATTERN (read (open-input-string lexeme)))]
      [(eof)
       (void)]))

(define-syntax-rule (declaration pattern _ result _)
		    [pattern result])

(define (token-read-syntax source reader-input)
 (with-syntax ([read-data (local-expand (parse (tokenize reader-input)) 'expression '())])
  (strip-context
   #'(module anything racket
       (provide tokenize)
       (require parser-tools/lex)
       (require (prefix-in : parser-tools/lex-sre))
       (require ragg/support)

       (define-syntax-rule (declaration pattern _ result _)
       			   [pattern result])

       (define-syntax-rule (tokenizer ip exp)
       	(begin
	  (port-count-lines! ip)
     	  (define my-lexer
	    exp)
    	  (define (next-token) (my-lexer ip))
    	  next-token))

       (define (tokenize ip)
         (tokenizer ip (syntax->datum read-data)))))))


(define (token-read ip) (syntax->datum (read-syntax ip)))

(provide (rename-out [token-read-syntax read-syntax]
	 	     [token-read read]))

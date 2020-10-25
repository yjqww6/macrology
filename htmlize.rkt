#lang racket

(require markdown syntax-color/module-lexer)

(define style
  #<<style
pre {
  word-wrap: break-word;
  white-space: pre-wrap;
}
pre.racket {
  background: rgb(240,240,240);
}
code.racket {
  font-family: 'Fira-Mono', monospace;
}
code.parenthesis {
  color: rgb(132,60,36);
}
code.symbol {
  color: rgb(38, 38, 128);
}
code.hash-colon-keyword {
  color: rgb(132, 60, 36);
}
code.string,code.constant {
  color: rgb(41, 128, 38);
}
code.comment,code.sexp-comment {
  color: rgb(192, 116, 31);
}
style
  )

(define (walk-xexpr xe proc)
  (define (loop xe [ctx '()])
    (or (proc xe loop ctx)
        (match xe
          [(list (? symbol? elem) (list (list (? symbol? name)
                                              (? string? val)) ...)
                 child ...)
           (list* elem (map list name val) (map (λ (xe)
                                                  (loop xe (cons elem ctx)))
                                                child))]
          [(list (? symbol? elem) child ...)
           (cons elem (map (λ (xe)
                             (loop xe (cons elem ctx)))
                           child))]
          [_ xe])))
  (map loop xe))

(define (pass f)
  (λ (xe)
    (walk-xexpr xe f)))

(define (coloring-code xe recur ctx)
  (define (types in)
    (let loop ([mode #f] [s '()])
      (define-values (str type _1 start end _4 new-mode)
        (module-lexer in 0 mode))
      (cond
        [(eof-object? str) (reverse s)]
        [(memq type
               '(comment sexp-comment constant
                         string parenthesis hash-colon-keyword symbol))
         (loop new-mode (cons (vector (sub1 start) (sub1 end) type str) s))]
        [else (loop new-mode s)])))

  (define (colorme str)
    (define bstr (string->bytes/utf-8 str))
    (define colored
      (types (open-input-bytes bstr)))

    (let loop ([pos 0] [colored colored])
      (match colored
        [(cons (vector start end type s) rest)
         (cond
           [(< pos start)
            (define bs (subbytes bstr pos start))
            (cons `(code ((class "racket")) ,(bytes->string/utf-8 bs))
                  (loop start colored))]
           [(= pos start)
            (cons `(code ((class ,(string-append "racket "
                                                 (symbol->string type))))
                         ,s)
                  (loop end rest))])]
        [_
         (cond
           [(>= pos (bytes-length bstr))
            '()]
           [else
            (list `(code ((class "racket")) ,(bytes->string/utf-8 (subbytes bstr pos))))])])))
  (define (racket? str)
    (member "racket" (string-split str)))
  (match xe
    [`(pre ((class ,(? racket? attr)) . ,other) (code ,_ ,str))
     `(pre ((class ,attr) . ,other) ,@(colorme str))]
    [`(code () ,str)
     #:when (not (member 'pre ctx))
     `(span () ,@(colorme str))]
    [_ #f]))

(define self-prefix
  (make-parameter "https://github.com/yjqww6/macrology/blob/master/"))

(define (replace-link xe recur ctx)
  (match xe
    [`(a ((href ,addr)) . ,child)
     #:when (string-prefix? addr (self-prefix))
     `(a ((href 
           ,(path->string
             (path-replace-extension (substring addr (string-length (self-prefix)))
                                     ".html"))))
         . ,child)]
    [_ #f]))

(define (htmlize str)
  `(html ()
         (head () (meta ((charset "utf-8")))
               (style () ,style))
         (body ()
               ,@((compose (pass replace-link)
                           (pass coloring-code)
                           parse-markdown)
                  str))))

(module+ main
  (require racket/cmdline racket/runtime-path net/sendurl)

  (define quite-mode (make-parameter #f))

  (define md
    (command-line
     #:program
     "htmlize"
     #:once-each
     [("-q" "--quiet") 
      "do not open browser"
      (quite-mode #t)]
     #:args md
     md))

  (for ([md (in-list md)])
    (define file (path-replace-extension md ".html"))
    (define html (htmlize (file->string md)))
  
    (display-to-file
     (xexpr->string html)
     file
     #:exists 'replace)
    (unless (quite-mode)
      (send-url/file (path->string file)))))


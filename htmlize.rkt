#lang racket

(require markdown syntax-color/module-lexer)

(define style
  #<<style
body {
  max-width: 1200px;
  margin-left: auto;
  margin-right: auto;
  font-family: "Open Sans","Clear Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
  line-height: 1.6;
  color: rgb(51,51,51);
}
div#whole {
  margin: 10px;
}
h1,h2 {
  border-bottom: solid thin lightgray;
}
ol,ul {
  margin: 0.8em 0;
  padding-left: 30px;
}
div.outer {
  border: solid 1px rgb(243,244,246);
  border-radius: 3px;
  margin: 10px 0 10px 0;
}
pre {
  margin: 0;
  word-wrap: break-word;
  white-space: pre-wrap;
  background: rgb(246,248,250);
  border: solid 1px rgb(239,241,242);
  border-radius: 3px;
  line-height: 1.3;
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
     `(div ((class "outer"))
           (pre ((class ,attr) . ,other) ,@(colorme str)))]
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

(define ((find-title box) xes)
  (for ([xe (in-list xes)])
    (match xe
      [`(h1 ,_ ,str)
       (set-box! box str)]
      [_ (void)]))
  xes)

(define (htmlize str)
  (define title (box ""))
  (define transformed
    ((compose (pass replace-link)
              (pass coloring-code)
              (find-title title)
              parse-markdown)
     str))
  `(html ()
         (head ()
               (title () ,(unbox title))
               (meta ((charset "utf-8")))
               (meta ((name "viewport")
                      (content "width=device-width, initial-scale=1.0, user-scalable=yes")))
               (style () ,style))
         (body ()
               (div ((id "whole"))
                    ,@transformed))))

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


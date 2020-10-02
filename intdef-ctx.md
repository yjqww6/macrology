# 如何使用First Class Internal Definition Context

Racket的first class internal definition context是一个利器，主要用途有：

* 可以用来对定义进行变换：

```racket
#lang racket

(define a%
  (class object%
    (define/match (fact n)
      [(0) 1]
      [(n) (* n (fact (sub1 n)))])
    (public fact)
    
    (super-new)))

(send (new a%) fact 5)
```

在这个例子中，类a%定义了一个fact方法，用的是define/match，而不需要一个特别定制的define-method宏。从函数定义到类的方法需要经过复杂的变换过程，但是define/match自身是不知道自己会被用来定义方法的。First class internal definition context使其成为了可能。

* 可以立即为局部的展开设置所需环境，而不需要推迟到后续的宏展开

## 应用示例

假设需要写这样一个宏：

```racket
(define-record (foo l)
  (match-define (list x _ y _ z) l))
```

需要展开为

```racket
(struct foo (x y z))
(define (make-foo l)
  (match-define (list x _ y _ z) l)
  (foo x y z))
```

也就是一次性从definition context中得到struct的字段和构造函数。

### 实现

#### 准备工作

从定义开始：

```racket
(define-syntax-parser define-record
  [(_ (name:id . args:formals) body:expr ...+)
   <...>])
```

这里无疑要从利用first class internal definition context（以下简称intdef-ctx）对body进行操作了，但首先，参数的args的绑定还没有设置好。

因此，上述的第二个用途，设置环境：

```racket
(define param-ctx (syntax-local-make-definition-context #f #f))
(syntax-local-bind-syntaxes (syntax->list #'args.params) #f param-ctx)
```

这里`parent-ctx`参数是`#f`，因为确实没有继承自其它的intdef-ctx；`add-scope?`参数也是`#f`，因为是设置环境，并不是真的引入了一个definition context，只是要防止后面的展开出现变量未定义的错误。



接下来定义body的intdef-ctx，这次就是常规状况了：

```racket
(define body-ctx (syntax-local-make-definition-context))
```

因为确实是一个definition context，所以`add-scope?`是默认的`#t`。那么`parent-ctx`为什么仍是默认的`#f`，不是param-ctx呢？因为body-ctx的定义并不需要加入到param-ctx中。



接下来看看怎么对body进行展开，首先是local-expand的使用：

```racket
(define ctx (list (gensym)))
(define (expand stx)
  (local-expand
   stx ctx
   (syntax->list #'(begin define-values define-syntaxes))
   (list body-ctx param-ctx)))
```

因为body的定义不需对外可见，`context-v`使用`(list gensym)`，否则可以用`generate-expand-context`。然后因为遇到的定义可能会相互或递归引用，必须部分展开，这里的`stop-ids`这三基本上是intdef-ctx展开不可少的，如果要其他特殊功能（例如，一个标记不需要变成结构体字段的定义的ignore宏），才会添加别的。body-ctx和param-ctx两个环境都要访问，因此都要传进去。

#### 递归展开

接下来要对body进行递归展开，先定义收集字段的变量：

```racket
(define defined-ids '())
```

开始遍历：

```racket
(let loop ([stx #'(begin body ...)])
  (syntax-parse (expand stx) #:literals (begin define-values define-syntaxes)
    <...>
  )) 
```

* 遇到begin的情况，直接递归：

```racket
[(begin form ...)
 #:with (expanded-form ...) (stx-map loop #'(form ...))
 #'(begin expanded-form ...)]
```

* 遇到define-values的情况：

```racket
[(define-values (ids ...) expr)
 #:with (bd ...) (stx-map syntax-local-identifier-as-binding #'(ids ...))
 (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #f body-ctx)
 (set! defined-ids (append (syntax->list #'(bd ...)) defined-ids))
 #'(define-values (bd ...) expr)]
```

这里就需要对body-ctx操作了。首先syntax-local-identifier-as-binding是去除ids的use-site scope，为什么需要这个步骤呢？因为每次local-expand可能引入不同的use-site scope，要使ids对其他定义可见，必须要去除use-site scope。然后，用syntax-local-bind-syntaxes将去除了use-site scope的名字添加到body-ctx中。

* 遇到define-syntaxes的情况：

```racket
[(define-syntaxes (ids ...) expr)
 #:with (bd ...) (stx-map syntax-local-identifier-as-binding #'(ids ...))
 #:with rhs (local-transformer-expand #'expr 'expression null body-ctx)
 (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #'rhs body-ctx)
 #'(define-syntaxes (bd ...) rhs)]
```

这里和上面不一样的是expr会被马上执行，而且要做完全展开。因为body自身不是完全展开，所以define-record的结果里仍可能会有对这些局部定义的宏的引用。为了避免expr被展开两次，这里先做完全展开。

* 其他情况直接返回：

```racket
[form #'form]
```

#### 收尾

按照要求的结构返回syntax

```racket
#:with ctor-body <上面展开的结果>
#:with (field ...) defined-ids
#:with ctor (format-id #'name "make-~a" #'name)
#'(begin (struct name (field ...))
         (define (ctor . args)
           ctor-body
           (name field ...)))
```

#### 完整代码

```racket
#lang racket
(require syntax/parse/define
         (for-syntax syntax/parse/lib/function-header syntax/stx racket/syntax))

(define-syntax-parser define-record
  [(_ (name:id . args:formals) body:expr ...+)
   #:do
   [(define param-ctx (syntax-local-make-definition-context #f #f))
    (define body-ctx (syntax-local-make-definition-context))
    (syntax-local-bind-syntaxes (syntax->list #'args.params) #f param-ctx)
    (define ctx (list (gensym)))
    (define (expand stx)
      (local-expand
       stx ctx
       (syntax->list #'(begin define-values define-syntaxes))
       (list body-ctx param-ctx)))
    (define defined-ids '())
    
    (define-syntax-rule (syntax/track form)
      (syntax-case this-syntax ()
        [(head . _) (syntax-track-origin #'form this-syntax #'head)]))]
   
   #:with ctor-body
   (let loop ([stx #'(begin body ...)])
     (syntax-parse (expand stx) #:literals (begin define-values define-syntaxes)
       [(begin form ...)
        #:with (expanded-form ...) (stx-map loop #'(form ...))
        (syntax/track (begin expanded-form ...))]
       [(define-values (ids ...) expr)
        #:with (bd ...) (stx-map syntax-local-identifier-as-binding #'(ids ...))
        (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #f body-ctx)
        (set! defined-ids (append (syntax->list #'(bd ...)) defined-ids))
        (syntax/track (define-values (bd ...) expr))]
       [(define-syntaxes (ids ...) expr)
        #:with (bd ...) (stx-map syntax-local-identifier-as-binding #'(ids ...))
        #:with rhs (local-transformer-expand #'expr 'expression null body-ctx)
        (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #'rhs body-ctx)
        (syntax/track (define-syntaxes (bd ...) rhs))]
       [form #'form]))
   #:with (field ...) defined-ids
   #:with ctor (format-id #'name "make-~a" #'name)
   (syntax-property
    #'(begin (struct name (field ...))
             (define (ctor . args)
               ctor-body
               (name field ...)))
    'sub-range-binders
    (let ([name-len (string-length (symbol->string (syntax-e #'name)))])
      (list (vector (syntax-local-introduce #'ctor)
                    5 name-len 0.5 0.5
                    (syntax-local-introduce #'name)
                    0 name-len 0.5 0.5))))])
```

注意到这里还添加了syntax/track的定义以及sub-range-binders的属性，用来协助Check Syntax。相关会在“如何让DrRacket正确画出箭头”中介绍。

使用示例：

```racket
> (begin
    (define-record (foo l)
      (match-define (list x _ y _ z) l))

    (define f (make-foo (list 1 2 3 4 5)))
    (list (foo-x f)
          (foo-y f)
          (foo-z f)))
'(1 3 5)
```



## 其他事项

* 这里没有使用internal-definition-context-introduce，什么情况会用到？

  如果要让展开的结果和其他东西混在一起，并且想要能被访问，会需要用到。

* 目前相关API中没有outside-edge scope的处理，在未来可能会调整，见<https://github.com/racket/racket/issues/3251>


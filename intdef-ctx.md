# 如何使用First Class Internal Definition Context

注：本文使用8.2.0.7引入的新api。



Racket的 _first class internal definition context_ 是一个利器，主要用途有：

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

在这个例子中，类`a%`定义了一个`fact`方法，用的是`define/match`，而不需要一个特别定制的“define-method”宏。从函数定义到类的方法需要经过复杂的变换过程，但是`define/match`自身是不知道自己会被用来定义方法的。 _First class internal definition context_ 使其成为了可能。

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

也就是一次性从 _definition context_ 中得到`struct`的字段和构造函数。

### 实现

#### 准备工作

从定义开始：

```racket
(define-syntax-parser define-record
  [(_ (name:id . args:formals) body:expr ...+)
   <...>])
```

这里无疑要利用 _first class internal definition context_ （以下简称 _intdef-ctx_ ）对`body`进行操作了，但首先，参数的args的 _binding_ 还没有设置好。

因此，上述的第二个用途，设置环境：

```racket
(define param-ctx (syntax-local-make-definition-context))
(syntax-local-bind-syntaxes (syntax->list #'args.params) #f param-ctx)
```

这里`param-ctx`提供一个包含了args的 _binding_ 的环境，防止出现变量未定义或是访问到外层定义的同名 _binding_ 。



接下来定义`body`的 _intdef-ctx_ ：

```racket
(define body-ctx (syntax-local-make-definition-context param-ctx))
```

这里的`parent-ctx`是`param-ctx`，因为`body`中的 _binding_ 需要访问`args`。



接下来看看怎么对`body`进行展开，首先是`local-expand`的使用：

```racket
(define ctx (list (gensym)))
(define (expand stx)
  (local-expand
   (internal-definition-context-add-scopes param-ctx stx)
    ctx
    (syntax->list #'(begin define-values define-syntaxes))
    body-ctx))
```

因为`body`的定义不需对外可见，`context-v`使用`(list (gensym))`，否则可以用`generate-expand-context`。然后因为遇到的定义可能会相互或递归引用，必须部分展开，这里的`stop-ids`这三基本上是 _intdef-ctx_ 展开不可少的，如果要其他特殊功能（例如，一个标记不需要变成结构体字段的定义的“ignore”宏），才会添加别的。因为`parent-ctx`的scope不会自动添加到`stx`中，这里需要手动调用`internal-definition-context-add-scopes`添加。

#### 递归展开

接下来要对`body`进行递归展开，先定义收集字段的变量：

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

* 遇到`begin`的情况，直接递归：

```racket
[(begin form ...)
 #:with (expanded-form ...) (stx-map loop #'(form ...))
 #'(begin expanded-form ...)]
```

* 遇到`define-values`的情况：

```racket
[(define-values (ids ...) expr)
 #:with (bd ...) (stx-map as-binding #'(ids ...))
 (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #f body-ctx)
 (set! defined-ids (append (syntax->list #'(bd ...)) defined-ids))
 #'(define-values (bd ...) expr)]
```

`as-binding`的定义为

```racket
(define (as-binding id)
  (syntax-local-identifier-as-binding id body-ctx))
```

这里就需要对`body-ctx`操作了。首先`syntax-local-identifier-as-binding`是去除`ids`的 _use-site scope_ ，为什么需要这个步骤呢？因为每次`local-expand`可能引入不同的 _use-site scope_ ，要使`ids`对其他定义可见，必须要去除 _use-site scope_ 。然后，用`syntax-local-bind-syntaxes`将去除了 _use-site scope_ 的名字添加到`body-ctx`中。

* 遇到`define-syntaxes`的情况：

```racket
[(define-syntaxes (ids ...) expr)
 #:with (bd ...) (stx-map as-binding #'(ids ...))
 #:with rhs (local-transformer-expand #'expr 'expression null body-ctx)
 (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #'rhs body-ctx)
 #'(define-syntaxes (bd ...) rhs)]
```

这里和上面不一样的是`expr`会被马上执行，而且要做完全展开。因为`body`自身不是完全展开，所以`define-record`的结果里仍可能会有对这些局部定义的宏的引用。为了避免`expr`被展开 __两次__ ，这里先做完全展开。

* 其他情况直接返回：

```racket
[form #'form]
```

#### 收尾

按照要求的结构返回syntax对象。

注意这里的`args-scoped`，需要使用`internal-definition-context-add-scopes`让`args`带上`param-ctx`的scope，然后body的名字才能解析到新的定义。

这是因为`args`是从宏的参数提供的，原本不含有`param-ctx`的scope。而`syntax-local-bind-syntaxes`会把intdef-ctx参数的scope添加到创建的 _binding_ 中。

因此，如果直接将`args`放入结果中，最终的scope set不是`param-ctx`里对应的 _binding_ 的scope set的超集，将导致“ambigious binding”。

相对地，后面`syntax-local-bind-syntaxes`所用的 _identifier_ 是从展开结果中获取的，需要`syntax-local-identifier-as-binding`。

```racket
#:with ctor-body <上面展开的结果>
#:with (field ...) defined-ids
#:with ctor (format-id #'name "make-~a" #'name #:subs? #t)
#:with args-scoped (internal-definition-context-add-scopes param-ctx #'args)
#'(begin (struct name (field ...))
         (define (ctor . args-scoped)
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
   [(define param-ctx (syntax-local-make-definition-context))
    (define body-ctx (syntax-local-make-definition-context param-ctx))
    (syntax-local-bind-syntaxes (syntax->list #'args.params) #f param-ctx)
    (define ctx (list (gensym)))
    (define (expand stx)
      (local-expand
       (internal-definition-context-add-scopes param-ctx stx)
       ctx
       (syntax->list #'(begin define-values define-syntaxes))
       body-ctx))
    (define defined-ids '())
    
    (define-syntax-rule (syntax/track form)
      (syntax-case this-syntax ()
        [(head . _) (syntax-track-origin #'form this-syntax #'head)]))

    (define (as-binding id)
      (syntax-local-identifier-as-binding id body-ctx))]
   
   #:with ctor-body
   (let loop ([stx #'(begin body ...)])
     (syntax-parse (expand stx) #:literals (begin define-values define-syntaxes)
       [(begin form ...)
        #:with (expanded-form ...) (stx-map loop #'(form ...))
        (syntax/track (begin expanded-form ...))]
       [(define-values (ids ...) expr)
        #:with (bd ...) (stx-map as-binding #'(ids ...))
        (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #f body-ctx)
        (set! defined-ids (append (syntax->list #'(bd ...)) defined-ids))
        (syntax/track (define-values (bd ...) expr))]
       [(define-syntaxes (ids ...) expr)
        #:with (bd ...) (stx-map as-binding #'(ids ...))
        #:with rhs (local-transformer-expand #'expr 'expression null body-ctx)
        (syntax-local-bind-syntaxes (syntax->list #'(bd ...)) #'rhs body-ctx)
        (syntax/track (define-syntaxes (bd ...) rhs))]
       [form #'form]))
   #:with (field ...) defined-ids
   #:with ctor (format-id #'name "make-~a" #'name #:subs? #t)
   #:with args-scoped (internal-definition-context-add-scopes param-ctx #'args)
   #'(begin (struct name (field ...))
            (define (ctor . args-scoped)
              ctor-body
              (name field ...)))])
```

注意到这里还添加了`syntax/track`的定义，用来协助Check Syntax。相关会在[如何让DrRacket正确画出箭头](https://github.com/yjqww6/macrology/blob/master/draw-arrow.md)中介绍。

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

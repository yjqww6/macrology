# 如何设置编译期信息

在Racket中，设置编译期信息，并可以被宏利用的手段，主要分为几类：

* 编译期的副作用
* Transformer Binding
* Syntax Property

## 副作用

在编译期使用带有副作用的代码是最灵活的手段，例如统计一个宏调用的次数：

```racket
#lang racket

(define-for-syntax a-counter (box 0))
(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     (begin
       (set-box! a-counter (add1 (unbox a-counter)))
       #'expr)]))
(define-syntax (report-a stx)
  (syntax-case stx ()
    [(_) (datum->syntax stx (unbox a-counter))]))

(a 1) ;1
(a 2) ;2
(report-a) ;2
```

只要在phase 1的代码里添加副作用，后续的宏就能看到该副作用，这样就设置了编译期的信息。

但是，这里需要注意几个问题：

### The Separate Compilation Guarantee

在DrRacket里运行上述代码后，在repl中输入`(report-a)`：

```racket
> (report-a)
0
```

这时因为每次一个module编译时，其phase 1的实例都是新创建的，其在phase 1引用的module也是如此。程序展开时的`a-counter`，和repl或者submodule展开时的`a-counter`，已经不是同一个了。这个是Racket的“The Separate Compilation Guarantee”，目的是为了各个module独立编译的一致性。

上面的实现中，`a`展开后，虽然产生了修改`a-counter`的副作用，但这个副作用并没有留到展开后的程序里。

解决这个问题的思路是并非直接在宏里执行副作用代码，而是让副作用的代码保留在宏展开后的结果里，作为module的phase 1初始化的一部分。这样module被其他地方使用的时候都能重新执行一遍，得到等价的结果。

```racket
#lang racket

(define-for-syntax a-counter (box 0))
(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     #'(begin
         (begin-for-syntax
           (set-box! a-counter (add1 (unbox a-counter))))
         expr)]))
(define-syntax (report-a stx)
  (syntax-case stx ()
    [(_) (datum->syntax stx (unbox a-counter))]))

(a 1)
(a 2)
(report-a)
```

注意到`begin-for-syntax`只能在module level或者top-level使用，如果要在 _internal definition context_ 使用，可以改成

```racket
(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     #'(begin
         (define-syntaxes ()
           (begin
             (set-box! a-counter (add1 (unbox a-counter)))
             (values)))
         expr)]))
```

### Generative和3D Syntax

通过展开为phase 1的代码达成副作用时，有时可能直接把某种struct放入生成的Syntax Object里：

```racket
#lang racket

(begin-for-syntax
  (struct A (v)))

(define-for-syntax a (box #f))

(define-syntax (set-a! stx)
  (syntax-case stx ()
    [(_ v)
     (begin
       (define x (A (syntax->datum #'v)))
       #`(begin-for-syntax
           (set-box! a '#,x)))]))

(define-syntax (get-a stx)
  (syntax-case stx ()
    [(_)
     #`'#,(A-v (unbox a))]))

(set-a! 10)
(get-a)
```

同样，在DrRacket中运行，显示`10`；然后到repl里：

```racket
> (get-a)
A-v: contract violation
  expected: A?
  given: #<A>
```

这是因为`struct`是“generative”的，每次运行都产生了新的定义，repl中的`A`和原来module展开时的`A`，已经不是同一种结构了。

使用`(struct A (v) #:prefab)`可以回避这个问题，但仍是治标不治本，因为所用结构的定义不一定都是受自己控制的。

正确的做法是不直接在代码中放入所需的数据结构，而是展开为构造这个结构的代码：

```racket
(define-syntax (set-a! stx)
  (syntax-case stx ()
    [(_ v)
     #'(begin-for-syntax
         (set-box! a (A v)))]))

(define-syntax (get-a stx)
  (syntax-case stx ()
    [(_)
     (datum->syntax stx (A-v (unbox a)))]))
```

这样，phase 1的代码构造的`A`就是当前可见的`A`。



实际上，在Syntax Object里放入无法marshal的值，得到的是一个 _3D Syntax_ ，无法序列化或编译到文件。修改前的`A`就是一个例子。

Typed Racket中的`Syntax`（即`(Syntaxof Syntax-E)`）代表可以序列化的 _2D Syntax_ ，而`(Syntaxof Any)`则包括了两者。

_3D Syntax_ 有其用途，例如如果要编写一个调试器，就可以直接在Syntax Object里放入一个函数，让代码和UI进行跨越phase、namespace以及module registry的交互。而一个通用的宏，则应该避免使用 _3D Syntax_ ，至少不能被保留到 _Fully Expanded Program_ 里。

#### 另一个例子

另一个可能比较有代表性的例子是scope的introducer，下面的`within-a`实现了类似C++的命名空间效果：

```racket
#lang racket

(begin-for-syntax
  (define a (make-syntax-introducer #t)))

(define-syntax (within-a stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([(body ...) (a #'(body ...))])
       #'(begin body ...))]))

(define x 0)

(within-a
 (define x 1))

(displayln x) ;0
(within-a
 (displayln x)) ;1
```

同理，由于`(make-syntax-introducer)`是“generative”的，在repl中就失效了：

```racket
> (within-a x)
0
```

使用 _3D Syntax_ ，直接把`a`放进Syntax Object能有所改善：

```racket
(define-syntax (define-intro stx)
  (syntax-case stx ()
    [(_ id)
     #`(begin-for-syntax
         (define id '#,(make-syntax-introducer #t)))]))

(define-intro a)
```

但正如上文所说， _3D Syntax_ 也有其自身的问题。这里正确的做法是使用`make-syntax-delta-introducer`。

```racket
#lang racket

(define-syntax (define-intro stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([scopeless (datum->syntax #f 'k)])
       (with-syntax ([scoped ((make-syntax-introducer #t)
                              #'scopeless)])
         #'(begin-for-syntax
             (define id (cons #'scoped #'scopeless)))))]))

(define-intro a)

(define-syntax (within-a stx)
  (syntax-case stx ()
    [(_ body ...)
     (let ([intro (make-syntax-delta-introducer
                   (car a) (cdr a))])
       (with-syntax ([(body ...) (intro #'(body ...))])
         #'(begin body ...)))]))

(define x 0)

(within-a
 (define x 1))

(displayln x)
(within-a
 (displayln x))
```

通过一对 _identifier_ ，一个应用过introducer，另一个没有，`make-syntax-delta-introducer`能够还原出最初的introducer。而 _identifier_ ，属于 _2D Syntax_ 。

可以看出，`make-syntax-delta-introducer`提供了用 _2D Syntax_ 表示任何“introducer”的方法。



### 局部的宏

局部的宏里，副作用的使用收到诸多限制：

* Definition Context的两轮展开

  ```racket
  (let ()
    (a 1)
    (a 2)
    (void))
  (report-a)
  ```

  注意到上述代码中，`let`里面的`a`没有被计入`(report-a)`计入，这跟Definition Context的两轮展开有关系，可见[部分展开 vs 完全展开](https://github.com/yjqww6/macrology/blob/master/local-expand.md#%E9%83%A8%E5%88%86%E5%B1%95%E5%BC%80-vs-%E5%AE%8C%E5%85%A8%E5%B1%95%E5%BC%80)。

  一种变通手法是将`(report-a)`改为`(#%expression (report-a))`，将展开延迟到第二轮。这也并非万全之策，应副作用的顺序影响结果。

* 表达式上下文

  在表达式上下文里，`begin-for-syntax`和`define-syntaxes`都无法使用，只能回到一开始的直接在宏里执行副作用。因此在设计宏的时候，应避免局部的副作用对外部可见。

另一方面，局部的副作用也通常会被一个使用`local-expand`做完全展开的宏消费掉，从而把副作用限制起来。



## Transformer Binding

`define-syntax`可以定义宏，但并不局限于定义宏，因为可以用`syntax-local-value`访问`define-syntax`定义的值，所以这也是一种通用定义编译期信息的手段。`define-syntax`定义的 _binding_ 在宏展开时可见，被称为 Transformer Binding_  。

```racket
#lang racket

(define-syntax x 1)
(define-syntax y 2)

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (if (eq? 1 (syntax-local-value #'id))
         #''foo
         #''bar)]))

(f x)
(f y)
```

这里`x`和`y`是 _Transformer Binding_ ，`f`在展开时可以检查`id`的值是否为1，并选择对应的宏展开方法。

这种方式的另一个优点是可以简单地用于local的binding：

```racket
(let-syntax ([z 1])
  (f z))
```

### 一名多用

有时候会希望定义的名字不仅用于记录信息，也可用作为宏或者普通函数使用。

这个时候可以使用`prop:procedure`，这个 _structure type property_ 可以让struct能够作为函数使用：

```racket
#lang racket

(begin-for-syntax
  (struct info (data proc)
    #:property prop:procedure (struct-field-index proc)))

(define-syntax x (info 1 (syntax-rules ()
                           [(_) 11])))
(define-syntax y (info 2 (syntax-rules ()
                           [(_) 22])))

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (if (eq? 1 (info-data (syntax-local-value #'id)))
         #''foo
         #''bar)]))

(f x)
(f y)

(let-syntax ([z (info 1 (syntax-rules ()))])
  (f z))

(x)
(y)
```

这样，当`x`、`y`和`z`作为宏使用时，调用的是`proc`字段的函数。可以得到

```racket
'foo
'bar
'foo
11
22
```

### 提高可组合性

上面的struct用法有一个缺点，当两个不相关的库分别定义了`info1`和`info2`时，无法同时把一个名字用于他们的binding，因为一个值不能同时既是`info1`类型又是`info2`类型。因此，需要用 _structure type property_ 作为信息设置的接口。

```racket
#lang racket

(begin-for-syntax
  (define-values (prop:info info? info-ref)
    (make-struct-type-property 'info))

  (define (info data proc)
    (struct info (data proc)
      #:property prop:info (struct-field-index data)
      #:property prop:procedure (struct-field-index proc))
    (info data proc)))

(define-syntax x (info 1 (syntax-rules ()
                           [(_) 11])))
(define-syntax y (info 2 (syntax-rules ()
                           [(_) 22])))

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (if (eq? 1 (info-ref (syntax-local-value #'id)))
         #''foo
         #''bar)]))

(f x)
(f y)

(let-syntax ([z (info 1 (syntax-rules ()))])
  (f z))

(x)
(y)
```

这里定义了`prop:info`作为接口，这样不需要固定使用某种struct，而是可以使用任何满足该property的值。

同理，如果要同时用于`info1`和`info2`，只需要

```racket
(struct myinfo (a b)
  #:property prop:info1 (struct-field-index a)
  #:property prop:info2 (struct-field-index b))
```

### 动态作用域

_Syntax Parameter_ 提供编译期的动态作用域，可以把它想象为`local-expand`和普通的 _Parameter_ 的组合，但它使用的是 _Transformer Binding_ 。

```racket
#lang racket
(require racket/stxparam)

(define-syntax-parameter x 1)
(define-syntax-parameter y 2)

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (if (eq? 1 (syntax-parameter-value #'id))
         #''foo
         #''bar)]))

(define-syntax-rule (foo-y expr)
  (syntax-parameterize ([y 1])
    expr))

(f x) ;'foo
(f y) ;'bar

(foo-y (f y)) ;'foo
```



### 替代手段：副作用和free-id-table

free-id-table配合副作用，可以可以把信息关联到名字上，能代替 _Transformer Binding_ 。更加方便的是，它可以把信息关联到已有的，甚至是其他module定义的名字上。同时由于局部的 _binding_ 对外不可见，也不用担心局部的副作用顺序问题。

```racket
#lang racket

(begin-for-syntax
  (require syntax/id-table racket/syntax)
  (define table (make-free-id-table)))

(define x 'foo)
(define y 'bar)

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (if (eq? 1 (free-id-table-ref table #'id))
         #''ok
         #''bad)]))

(define-syntax (attach! stx)
  (syntax-case stx ()
    [(_ id value)
     #'(define-syntaxes ()
         (begin
           (free-id-table-set! table #'id value)
           (values)))]))


(attach! x 1)
(attach! y 2)

(f x)
(f y)
```

这种方法的缺陷是可以重复把信息关联到同一个名字上，造成隐患。但反过来说，这种特性也方便了增量式开发。

#### 避免依赖

如果需要把信息关联到其他module的名字里：

```racket
#lang racket/base
(require (for-syntax racket/base
                     syntax/id-table racket/syntax))

(begin-for-syntax
  (define table (make-free-id-table)))

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id)
     (if (eq? 1 (free-id-table-ref table #'id))
         #''ok
         #''bad)]))

(define-syntax (attach! stx)
  (syntax-case stx ()
    [(_ id value)
     #'(define-syntaxes ()
         (begin
           (free-id-table-set! table #'id value)
           (values)))]))

(require racket/match)
(attach! match 1)
(f match)
```

没有问题，但是这也带来了对`racket/match`的依赖，即便没有`(f match)`也是如此。若不`(require racket/match)`，则无法建立正确的binding：

```racket
(attach! match 1)
(module+ main
  (require racket/match)
  (f match))
;; free-id-table-ref: no mapping for #<syntax:unsaved-editor:32:5 match>
```

这个时候可以使用Racket 7引入的`syntax-binding-set`：

```racket
(define-syntax (attach-loosely! stx)
  (syntax-case stx ()
    [(_ mod id value)
     (with-syntax ([id (syntax-binding-set->syntax
                        (syntax-binding-set-extend
                         (syntax-binding-set)
                         (syntax->datum #'id)
                         (variable-reference->module-base-phase
                          (#%variable-reference))
                         (module-path-index-join (syntax->datum #'mod) #f))
                        (syntax->datum #'id))])
       #'(begin-for-syntax
           (free-id-table-set! table #'id value)))]))

(attach-loosely! racket/match/match match 1)
(module+ main
  (require racket/match)
  (f match))
```

这种方法自身不会引入依赖，只有用户确实要用到`match`的时候才会引入依赖。

另外，注意到这里的的mod参数是`racket/match/match`，这是一个缺点，它需要精确知道`match`的定义所在的module，而不仅是`require`所需的module。

---

在Racket7之前的版本中，也可以采用取巧的方法实现这个功能。Typed Racket源码中有一个类似的`make-template-identifier`函数可以参考：

```racket
(define (make-template-identifier what where)
  (let ([name (module-path-index-resolve (module-path-index-join where #f))])
    (parameterize ([current-namespace (make-empty-namespace)])
      (namespace-attach-module (current-namespace) ''#%kernel)
      (parameterize ([current-module-declare-name name])
        (eval `(,#'module any '#%kernel
                 (#%provide ,what)
                 (define-values (,what) #f))))
      (namespace-require `(for-template ,name))
      (namespace-syntax-introduce (datum->syntax #f what)))))
```



## Syntax Property

### Reader和Syntax Property

Reader可以添加syntax property，典型例子有`'paren-shape`和`syntax-original?`。另外，Typed Racket也定义了一系列用于附加类型信息的Reader扩展。

示例（保存为”a.rkt“）：

```racket
#lang racket/base
(provide (rename-out [read-syntax* read-syntax]))

(define (read-syntax* source in)
  (define stx (read-syntax source in))
  (define sym (syntax->datum (read-syntax source in)))
  (define val (syntax->datum (read-syntax source in)))
  (syntax-property stx sym val))
```

```racket
#lang racket

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ x)
     (if (syntax-property #'x 'readed!)
         #'1
         #'x)]))

(f #reader "a.rkt"100 readed! #t) ;; 1
(f 100) ;; 100
```

```racket
#lang typed/racket

(define #reader "a.rkt" f type-label (Option Integer) 100)

;; > f
;; - : (U False Integer)
;; 100
```

有时候需要让Syntax Property的值也是一个Syntax Object，并且拥有与所属的Syntax Object一样的上下文信息，那么Reader直接附加信息的方法就不太适用了。

替代方案是Reader将其改写为宏调用，再在宏里附加Syntax Property，这是Typed Racket对`#{e :: t}`到`(ann e t)`形式的处理方法。这种方法有不卫生的风险：

```racket
#lang typed/racket
(define ann 1)
#{1 :: Integer}
```

这个Typed Racket程序因为Reader引入的`ann`解析到用户定义的那个，所以不能正常工作。

### 宏和Syntax Property

由于宏展开是由外到内的，正常情况下一个宏是不能利用作为参数的表达式的Syntax Property的。

```racket
#lang racket

(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-property #'expr 'foo 'bar)]))

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax ([foo (syntax-property #'expr 'foo)])
       #''foo)]))

(f (a 1))
```

只能得到`#f`。

如果要利用宏附加的Syntax Property，基本上有两种途径：

1. 在 _Fully Expanded Program_ 上操作，例子是DrRacket的Check Syntax。（另见[如何让DrRacket正确地画出箭头](https://github.com/yjqww6/macrology/blob/master/draw-arrow.md)）

   遍历一个 _Fully Expanded Program_ 的方法可以看[Phase 与 identifier的匹配](https://github.com/yjqww6/macrology/blob/master/phase-match.md#%E5%8C%B9%E9%85%8Dfully-expanded-program)。

2. 外层的宏使用`local-expand`。（另见[local-expand该怎么用](https://github.com/yjqww6/macrology/blob/master/local-expand.md)）

   这方面最典型的例子是[Type Systems as Macros](https://www.ccs.neu.edu/home/stchang/pubs/ckg-popl2017.pdf)。

下面讨论`local-expand`的情况需要注意的点。

#### 展开为表达式

就像在[local-expand该怎么用](https://github.com/yjqww6/macrology/blob/master/local-expand.md)提到的，完全展开要延迟到 _expression context_ ，上面的`f`需要改为：

```racket
#lang racket
(require (for-syntax syntax/transformer racket/syntax))

(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-property #'expr 'foo 'bar)]))

(define-syntax f
  (make-expression-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ expr)
        (with-syntax*
            ([expr (local-expand #'expr 'expression null)]
             [foo (syntax-property #'expr 'foo)])
          #''foo)]))))

(f (a 1))
```

可以得到`'bar`。

#### 展开为定义
同样，当`f`展开为定义时，是不能用完全展开的：

```racket
#lang racket
(require (for-syntax syntax/transformer racket/syntax))

(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-property #'expr 'foo 'bar)]))

(define-syntax (f stx)
  (syntax-case stx ()
    [(_ id expr)
     (with-syntax*
         ([expr (local-expand #'expr 'expression null)]
          [foo (syntax-property #'expr 'foo)])
       #'(define id 'foo))]))

(f x (a (dummy)))

(define (dummy) 0)
```

会导致“unbound identifier”。针对上面的例子，可以用一个辅助宏延迟展开：

```racket
(define-syntax (f-helper stx)
  (syntax-case stx ()
    [(_ expr)
     (with-syntax*
         ([expr (local-expand #'expr 'expression null)]
          [foo (syntax-property #'expr 'foo)])
       #''foo)]))

(define-syntax-rule (f id expr)
  (define id (f-helper expr)))
```

#### 按名字传播Syntax Property

有时候需要模仿关联到名字的方式，让对一个名字的使用都带有某个Syntax Property，这个时候就要用“variable-like”宏了。

```racket
#lang racket
(require (for-syntax syntax/transformer racket/syntax))

(define-syntax (a stx)
  (syntax-case stx ()
    [(_ expr)
     (syntax-property #'expr 'foo 'bar)]))

(define-syntax f
  (make-expression-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ expr)
        (with-syntax*
            ([expr (local-expand #'expr 'expression null)]
             [foo (syntax-property #'expr 'foo)])
          #''foo)]))))

(define-syntax (define-a stx)
  (syntax-case stx ()
    [(_ id expr)
     #'(begin
         (define tmp expr)
         (define-syntax (id stx)
           (syntax-case stx ()
             [x (identifier? #'x) #'(a tmp)])))]))

(define-a x 0)
(displayln (f x))
```


# 可扩展的宏

（早期版本：<https://zhuanlan.zhihu.com/p/35953489> <https://zhuanlan.zhihu.com/p/36709037> )

一些宏中会提供扩展机制，例如 `require` 、`provide` 、`for` 、`match` 以及 `syntax-parse`。

其特征是需要用户自行定义一个宏A，并把A的名字提供给这些宏。A的过程会被调用，结果会被用于下一步的操作。

一个例子：

```Racket
#lang racket
(define-match-expander succ
  (λ (stx)
    (syntax-case stx ()
      [(_ ?N)
       #'(app sub1 ?N)])))

(match 5
  [(succ (succ N)) N])
```

这里用户定义了`succ`（为什么会用 `define-match-expander` 而不是 `define-synax` 后面会说），对应的过程被`match`调用，最后匹配模式变成了 `(app sub1 (app sub1 N))` 。最终输出`3`。



## 天真的实现

因为宏绑定的过程可以用 `syntax-local-value` 获得，所以按直觉写出来的代码一般类似：

```racket
#lang racket

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     ((syntax-local-value #'id) #'in)]))

(use-expander expander1 car)
```

这里直接调用`expander1`的过程，而不是将其用于宏展开，最后展开为 `(car x)` ，结果得到`1`。

### 毛病

Racket的宏是卫生的，不是同一次的宏展开引入的名字默认不会意外绑定到彼此。然而，上面的方案中，`expander1`的调用仍处于`use-expander`的展开中，因此`expander1`引入的`x`是有可能出问题的。

```racket
#lang racket

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let ([x '(2)])
         #,((syntax-local-value #'id) #'in))]))

(use-expander expander1 car)
```

这里`x`绑定到了`use-expander`局部引入的`x`，最终运行得到`2`。

## 早期的解决方案

Racket社区早期的解决方案是手动模拟宏展开过程中的_macro-introduction scopes_的双重反转。至于Racket宏的卫生原理文档里早有论述，这里不多做说明，见：

* <https://www.cs.utah.edu/plt/scope-sets/>
* <https://docs.racket-lang.org/reference/syntax-model.html#%28part._transformer-model%29>
* <https://www.youtube.com/watch?v=Or_yKiI3Ha4>

```racket
#lang racket

(begin-for-syntax
  (define (apply-expander proc stx)
    (define introducer (make-syntax-introducer))
    (define intro-stx (introducer (syntax-local-introduce stx)))
    (syntax-local-introduce (introducer (proc intro-stx)))))

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'in))]))

(use-expander expander1 car)
```

这个方法的核心在于`apply-expander`过程：

1. 输入的`stx`首先被应用`syntax-local-introduce`，模拟`use-expander`的展开过程暂时结束，准备进入新一次的宏展开。
2. `introducer`被应用，模拟进入了新一次的宏展开，引入新的_macro-introduction scope_。
3. `proc`被应用，得到`expander1`的结果。
4. `introducer`再次被应用，模拟`expander1`的展开结束，反转此次的_macro-introduction scope_。
5. `syntax-local-introduce`再次被应用，回到`use-expander`的展开过程中。

这样，`use-expander1`的`x`被打上了唯一的_macro-introduction scope_，`expander1`的`x`不可能具有该_scope_，不会意外的绑定。

值得注意的是，这里`syntax-local-introduce`和`introducer`都是必要的：

* 缺少`syntax-local-introduce`的话，`expander1`的`x`会带上`use-expander`的_macro-introduction scope_，仍会绑定到`use-expander`的`x`。另一方面，缺少`syntax-local-introduce`也会对`disappeared-use`的处理造成麻烦，这里不展开描述。
* 缺少`introducer`的话，如果`use-expander`不止能调用`expander1`，还能调用更多的宏，例如`expander2`，那么`expander1`和`expander2`的结果可能意外地互相绑定。另外如果`expander1`引入定义，`use-expander`引入使用的话，也可能会出问题。

## 插曲：local-apply-transformer

在7.0版本（准确来说，6.90.0.29）中，Racket正式引入了`local-apply-transformer`这个api，它解决了什么问题呢？

如果上面的`expander1`也想要自己支持扩展功能，它能复用`apply-expander`过程吗？不能，因为`syntax-local-introduce`是属于`use-expander`的，`introduer`才是`expander1`自己的"syntax-local-introduce"。注意到上面的`introducer`是`apply-expander`的一个局部变量，这样`expander1`甚至无法访问到自己的`introducer`，自然也不能用来写 扩展了。

传统的解法是把`introducer`绑定到一个_parameter_里面，这样`expander1`就可以访问它了。这就是为什么会有 `syntax-local-match-introduce` , `syntax-local-require-introduce` , `syntax-local-provide-introduce` ,   `syntax-local-syntax-parse-pattern-introduce` , ……

但是这样`expander1`怎么知道以自己的身份到底该用哪个“introducer”呢？也许大家再共用一个“current-introduce”？这样就太容易遗漏了。

对于这个问题，`local-apply-transformer`的方案是不直接调用`expander1`的过程，而是通过`local-expand`将其作为一次宏展开间接地调用。这样，`expander1`就是一次普通的宏展开了，对应的`introducer`就是普通的“syntax-local-introduce”，上面那堆“xxx-introduce”也就不需要了。

因此，上面的写法演变为

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [id #'(id x)]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'in))]))

(use-expander expander1 car)
```

## 还有问题？

回到早期方案，来看看这个程序：

```racket
#lang racket
(begin-for-syntax
  (define (apply-expander proc stx)
    (define introducer (make-syntax-introducer))
    (define intro-stx  (introducer (syntax-local-introduce stx)))
    (syntax-local-introduce (introducer (proc intro-stx)))))

(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(an-x id) #'(let ([an-x '(3)])
                   (id x))]))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x in)))]))

(use-expander x expander1 car)
```

得到了`3`，为什么呢？

简单来说，在`apply-expander`时，因为`use-expander`的定义和使用在同一_definition context_，`syntax-local-introduce`反转的不仅有_macro-introduction scope_，还有_use-site scope_，因此`expander1`中的`x`被打上了_use-site scope_，然后_use-site scope_在宏展开结束时不会被反转。`an-x`和`x`都带有该_scope_，导致返回`3`。

但是对一个卫生的宏系统，这里应该得到`1`，就像直接调用 `(expand1 x car)` 那样。

我们无法直接模拟_use-site scope_（缺少一个检测宏的定义和使用是否在同一_definition context_的api），所以直接在早期方案上改进似乎无法解决这个问题。（注：一些失败的尝试可在<https://zhuanlan.zhihu.com/p/36709037>看到）

## local-apply-transformer可以解决问题吗？

将上面的`apply-expander`定义替换，得到（记为程序A）

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x in)))]))
  
(define x '(1))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(an-x id) #'(let ([an-x '(3)])
                   (id x))]))

(use-expander x expander1 car)
```

这次是`1`了，那么问题解决了吗？

并没有，把`expander1`的定义和使用套进`let`里（记为程序B），仍得到3

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx 'expression)))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let ([x '(2)])
         #,(apply-expander (syntax-local-value #'id) #'(an-x in)))]))
  
(define x '(1))

(let ()
  (define-syntax (expander1 stx)
    (syntax-case stx ()
      [(an-x id) #'(let ([an-x '(3)])
                     (id x))]))

  (use-expander x expander1 car))
```

为什么在A里是`1`？因为用`local-apply-transformer`的话，结果的_use-site scope_就跟普通的宏展开一样，不会被反转。这样，`expander1`里`(id x)`的`x`没有_use-site scope_。这就是它跟早期方案的不同。

那为什么在B里又变回`3`了？因为`use-expander`的定义和使用不在同一_definition context_了，所以也就都没有_use-site scope_了。

可以认为上面对`local-apply-transformer`的使用效果类似于

```racket
(begin-for-syntax
  ;;只反转macro-introduction scope
  (define (macro-scope-introducer)
    (make-syntax-delta-introducer
     (syntax-local-identifier-as-binding
      (syntax-local-introduce #'x))
     #'x))
  (define (apply-expander proc stx)
    (define macro-introducer (macro-scope-introducer))
    (define introducer (make-syntax-introducer))
    (define intro-stx  (introducer (macro-introducer stx)))
    (macro-introducer (introducer (proc intro-stx)))))
```

实际上，在此文写下的时间点，Racket标准库里也有这个问题：

```racket
#lang racket
(define x sub1)
(define-match-expander succ
  (lambda (stx)
    (syntax-case stx ()
      [(_ v)  #'(app (let ([v add1]) x) v)])))

(match 3
  [(succ x) x]) ;得到4，而不是2
```

### 到底错在哪里？

仔细观察会发现，`use-expander`的_use-site scope_并不完全是问题所在。

不妨先看看普通宏版本是什么情况：

```racket
#lang racket

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #'(let ([x '(2)])
         (id an-x in))]))
  
(define x '(1))
(let ()
  (define-syntax (expander1 stx)
    (syntax-case stx ()
      [(_ an-x id) #'(let ([an-x '(3)])
                       (id x))]))

  (use-expander x expander1 car))
```

这里`use-expander`先被展开，因此 `(let ([x '(2)]) ...)` 这里带有了`let`自己的local _scope_。同时，因为`(let ([an-x '(3)]) ...)` 这里的`an-x`是`use-expander`传来的，这样两个局部的`x`都带有该local _scope_。然而，`(id x)`的`x`是`expander1`新引入的，没有该local _scope_，所以只能绑定到module顶部的`x`。

另一方面，如果不要`use-expander`内部的`let`：

```racket
#lang racket

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #'(id an-x in)]))
  
(define x '(1))
(let ()
  (define-syntax (expander1 stx)
    (syntax-case stx ()
      [(_ an-x id) #'(let ([an-x '(3)])
                       (id x))]))

  (use-expander x expander1 car))
```

这里则是`expander1`自身定义和使用在同一_definition context_，`an-x`带有_use-site scope_，因此也能得到`1`。

所以真正的问题有两个：

1. 内部的`expander1`先于外部的`use-expander`被展开，以至内部被打上了本不该有的local _scope_。
2. `expander1`的展开没有适当地引入_use-site scope_。事实上，在_expression context_下，`local-apply-transformer`（或者说`local-expand`）永远不会引入_use-site scope_，这是与Racket 7之前的expander的一个差异。

这两个问题归根到底都是`local-expand`的问题，似乎可能通过把宏写成“trampoline”的形式回避，但相信没人会愿意这么写吧。

### 如果添加use-site scope

另一方面，如果把`local-apply-transformer`从_expression context_换到_internal definition context_，则会无条件引入_use-site scope_。

这么修改，A和B都能得到`1`。

可以认为效果类似于

```racket
(begin-for-syntax
  (define (macro-scope-introducer)
    (make-syntax-delta-introducer
     (syntax-local-identifier-as-binding
      (syntax-local-introduce #'x))
     #'x))
  (define (apply-expander proc stx)
    (define macro-introducer (macro-scope-introducer))
    (define introducer (make-syntax-introducer))
    (define use-site-introducer (make-syntax-introducer #t))
    (define intro-stx  (use-site-introducer (introducer (macro-introducer stx))))
    (macro-introducer (introducer (proc intro-stx)))))
```



但这并不意味着完全对了。在可扩展的宏中，模式匹配一类，也即是需要引入绑定的，的是最常见的。

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx (list (gensym)))))

(define x '(1))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let-values (#,(apply-expander (syntax-local-value #'id) #'an-x))
         in)]))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [an-x #'[(an-x) '(2)]]))

(use-expander x expander1 (car x))
```

这里还是得到`1`，但是期望应该是`2`。

因为在绑定的位置引入了_use-site scope_，导致`expander1`的x没能绑定到`(car x)`中的x。

`syntax-local-identifier-as-binding`可以消除_use-site scope_，所以如果事先知道结果的形状，还是有可能解决问题的：

```racket
#lang racket
(require (for-syntax syntax/apply-transformer syntax/stx))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx (list (gensym)))))

(define x '(1))

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ an-x id in)
     #`(let-values (#,(syntax-case (apply-expander (syntax-local-value #'id) #'an-x) ()
                        [[(x ...) rhs]
                         (with-syntax ([(x ...)
                                        (stx-map syntax-local-identifier-as-binding #'(x ...))])
                           #'[(x ...) rhs])]))
         in)]))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [an-x #'[(an-x) '(2)]]))

(use-expander x expander1 (car x))
```

这个方案其实就是 <https://github.com/racket/racket/pull/2237> 。



但是，对于不可预测的展开，情况就比较复杂了。考虑可以引入定义的展开（例如`syntax-parse`的`~do`）：

```racket
#lang racket
(require (for-syntax syntax/apply-transformer))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx (list (gensym)))))

(define x 1)

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ [defs ...] id in)
     #`(let ([x 2])
         #,(apply-expander (syntax-local-value #'id) #'(id defs ...))
         in)]))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(id defs ...) #'(begin defs ...)]))

(use-expander [(define x 3)] expander1 x)
```

这里结果是`1`，但期望应该是`3`。一种修复方法是再使用部分展开

```racket
#lang racket
(require (for-syntax syntax/apply-transformer
                     syntax/kerncase syntax/stx))

(begin-for-syntax
  (define (apply-expander proc stx)
    (local-apply-transformer proc stx (list 'gensym)))

  (define (remove-binder-use-site stx)
    (define ctx (syntax-local-make-definition-context #f #f))
    (define c (list 'gensym))
    (define (expand stx)
      (local-expand stx c (kernel-form-identifier-list) ctx))
    (let loop ([stx (expand stx)])
      (syntax-case stx (define-values define-syntaxes begin)
        [(begin form ...)
         (with-syntax ([(form ...) (stx-map (compose loop expand) #'(form ...))])
           #'(begin form ...))]
        [(define-values (id ...) expr)
         (begin
           (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f ctx)
           (with-syntax ([(id ...) (stx-map syntax-local-identifier-as-binding #'(id ...))])
             #'(define-values (id ...) expr)))]
        [(define-syntaxes (id ...) expr)
         (with-syntax ([expr (local-transformer-expand #'expr 'expression null)])
           (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'expr ctx)
           (with-syntax ([(id ...) (stx-map syntax-local-identifier-as-binding #'(id ...))])
             #'(define-syntaxes (id ...) expr)))]
        [_ stx])))
  )

(define x 1)

(define-syntax (use-expander stx)
  (syntax-case stx ()
    [(_ [defs ...] id in)
     #`(let ([x 2])
         #,(remove-binder-use-site
            (apply-expander (syntax-local-value #'id) #'(id defs ...)))
         in)]))

(define-syntax (expander1 stx)
  (syntax-case stx ()
    [(id defs ...) #'(begin defs ...)]))

(use-expander [(define x 3)] expander1 x)
```

这种方法需要仔细区分各种expander预期的上下文。

## 结论

对于这个问题，现在仍没有完美的解决方案。

* 如果不需要引入绑定， `(local-apply-transformer proc stx (list (gensym)))` 比较合适。
* 如果需要引入绑定，
  * 可以接受一些风险，可以使用 `(local-apply-transformer proc stx 'expression)` ；
  * 如果能忍受一些麻烦，可以用 `(local-apply-transformer proc stx (list (gensym)))` 配合`syntax-local-identifier-as-binding`。

## 其他相关问题

### 不要使用syntax-rules
编写`expander1`的时候，注意不要使用`syntax-rules`，因为其会对结果应用`syntax-protect`。而一般写`use-expander`这类宏的时候，是不会特意去`syntax-disarm`的，因此结果会处于_tainted_状态。

### define vs attach

见<https://rmculpepper.github.io/blog/2013/06/define-vs-attach/>

### 为什么需要define-match-expander

因为会需要定义的名字在`match`内外有不同的行为。例如，在`match`外面使用时，直接报错。

因此需要把match-expander定义成一个struct，并利用_struct property_控制其行为。`prop:procedure`对应在外面使用的情况，`prop:match-expander`对应`match`内使用的情况。

那么为什么需要`prop:match-expander`，而不是固定使用某一字段呢？

因为struct可以有多个_struct property_，可以同时带有`prop:pattern-expander`等其他的，做到一个名字在不同宏里有不同意义。如果简单限定为一个字段，嵌套后会失去其他属性。

